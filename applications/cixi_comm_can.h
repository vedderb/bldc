#ifndef CAN_CIXI_COMMS_H
#define CAN_CIXI_COMMS_H

#include <stdint.h>
#include <stdbool.h>

#define LAST_BYTE_MASK      0xFFU
#define FIRST_TWO_BITS_MASK 0xC0U
#define SHIFT_8_BITS        8U
#define SHIFT_6_BITS        6U
#define MASK_CIXI_RECV_BASE 0x7F0

#define TORQUE_SCALE 0.1F
#define RPM_SCALE    0.1F
#define EE_SIG_SCALE 0.01F

#define MOTOR_TORQUE_CONSTANT 1.0F
#define BRAKE_CURRENT         5.0F
#define STOP_CURRENT          0.0F

#define CIXI_HEARTBEAT_TIMEOUT_MS               150U
#define CIXI_HEARTBEAT_SEND_INTERVAL_MS         35U
#define CIXI_STATUS_SEND_INTERVAL_MS            10U
#define CIXI_HEARBEAT_TIMEOUT_CHECK_INTERVAL_MS 25U

#define APPCONF_APP_TO_USE    APP_CUSTOM
#define APPCONF_SHUTDOWN_MODE SHUTDOWN_MODE_ALWAYS_ON

/**
 * @brief Enum for identifying base CAN IDs of known CIXI frames.
 */
typedef enum
{
    CIXI_CAN_CMD_BASE       = 0x030,
    CIXI_CAN_STATUS_BASE    = 0x038,
    CIXI_CAN_TEMP_BASE      = 0x12C,
    CIXI_CAN_ERROR_BASE     = 0x130,
    CIXI_CAN_WARNING_BASE   = 0x137,
    CIXI_CAN_VERSION_BASE   = 0x7C6,
    CIXI_CAN_HEARTBEAT_BASE = 0x7C0,
    CIXI_CAN_PERS_HEARTBEAT = 0x780
} CixiCanFrameIdBase;

/**
 * @brief Controller state.
 *
 * Used to inform the PERS system about the current state of the VESC
 */
typedef enum
{
    CIXI_STATE_INIT    = 0U, /**< Initial state */
    CIXI_STATE_STANDBY = 1U, /**< Standby mode: system ready but idle */
    CIXI_STATE_ACTIVE  = 2U, /**< Active state: torque command is valid */
    CIXI_STATE_ERROR   = 3U  /**< Error/faulted state */
} CixiControllerState;

/**
 * @brief Control enable state values from CIXI command frame.
 *
 * Matches values from bits 0–1 of byte 2 in the command frame.
 * Defined on bit index 22 (2 bits) in the DBC.
 */
typedef enum
{
    CIXI_ENABLE_STANDBY = 0U, /**< Standby mode: system ready but idle */
    CIXI_ENABLE_ACTIVE  = 3U  /**< Active state: torque command is valid */
} CixiEnableSignal;

/**
 * @brief Enum for multiplexed pages in the CIXI status frame.
 */
typedef enum
{
    STATUS_PAGE_1 = 0,
    STATUS_PAGE_2 = 1,
    STATUS_PAGE_3 = 2
} CixiStatusPage;

/**
 * @brief Decoded representation of a CIXI Propulsion Controller Status
 * frame.
 */
typedef struct
{
    int16_t control_value_in; ///< Nm
    int16_t motor_torque;     ///< Nm
    int16_t rpm_sensored;     ///< RPM (scaled)
    float   electrical_power; ///< W
    float   mechanical_power; ///< W
    float   input_voltage;    ///< V
    float   input_current;    ///< A
    float   iq;               ///< A
    uint8_t status_page;      ///< Which PAGE to send (0–2)
    uint8_t control_state;    ///< Enum: INIT, STANDBY, ACTIVE, ERROR
    bool    valid;            ///< True if frame was successfully decoded
} CixiCanData;

/**
 * @brief Decoded representation of a CIXI Command frame (PERS ➜
 * Controller).
 */
typedef struct
{
    int16_t          control_value;  ///< Torque command (Nm)
    CixiEnableSignal control_enable; ///< 0 = STANDBY, 3 = ACTIVE
    uint8_t          control_incr;   ///< Command increment
    bool             valid;          ///< Set to true if parsed successfully
} CixiCanCommand;

/**
 * @brief Decoded representation of a PERS heartbeat frame.
 */
typedef struct
{
    uint16_t  heartbeat_counter;
    systime_t heartbeat_time;
    bool      valid;
} CixiCanHeartbeat;

/**
 * @brief Return Type fo Cixi CAN operations.
 */
typedef enum
{
    CIXI_CAN_OK    = 0,
    CIXI_CAN_ERROR = 1
} CixiCanStatus;

/**
 * @brief Process an incoming CIXI CAN frame (PERS ➜ Controller).
 *
 * Supports decoding of command and heartbeat frames.
 *
 * @param can_id CAN ID of the incoming frame.
 * @param data8 Pointer to the 8-byte CAN payload.
 * @param len Payload length.
 * @param is_ext True if frame came from external controller (must be true).
 * @return bool
 */
bool process_can_cixi_frame(uint32_t can_id, uint8_t *data8, uint8_t len);

/**
 * @brief Parse a PERS heartbeat frame (CAN ID 0x780).
 *
 * @param data8 Pointer to the 8-byte CAN payload.
 * @param len Payload length (should be 2).
 * @return A `CixiCanHeartbeat` struct with decoded values and validity
 * flag.
 */
CixiCanHeartbeat parse_pers_heartbeat(uint8_t *data8, int len);

/**
 * @brief Parse a CIXI Command frame sent from PERS to the controller.
 *
 * @param can_id CAN ID of the message (should match 0x030 + X).
 * @param data8 Pointer to the 8-byte CAN payload.
 * @param len Payload length (should be 4).
 * @return A `CixiCanCommand` struct with decoded values and validity flag.
 */
CixiCanCommand parse_cixi_cmd_frame(uint32_t can_id,
                                    uint8_t *data8,
                                    uint8_t  len);

/**
 * @brief Send a heartbeat frame to the PERS system over CAN.
 *
 * This transmits a 2-byte counter that increments with each call.
 *
 * @param controller_id Controller node ID (0–3) used in CAN ID.
 * @return MSG_OK if frame was sent successfully; error code otherwise.
 */
CixiCanStatus cixi_can_send_heartbeat(uint8_t controller_id);

/**
 * @brief Registers the CIXI CAN frame reception callback with the CAN
 * communication layer.
 *
 * This function configures the CAN reception system to use the
 * `process_can_cixi_frame` handler whenever a standard identifier (SID)
 * frame is received. It is required to initialize this before decoding CIXI
 * command or heartbeat frames.
 *
 * @return CIXI_CAN_OK on successful registration, CIXI_CAN_ERROR otherwise.
 */
CixiCanStatus setup_cixi_callback(void);

/**
 * @brief Constructs and transmits a CIXI propulsion controller status frame
 * over CAN.
 *
 * Based on the specified `status_page`, this function encodes the
 * appropriate subset of telemetry signals (e.g., torque, power, voltage)
 * into a 7-byte CAN frame as defined in the CIXI DBC specification. It
 * multiplexes data according to the page index and sends it using
 * `comm_can_transmit_sid`.
 *
 * The signal values in the `CixiCanData` struct are automatically scaled
 * and encoded according to the DBC's requirements (e.g., ×0.1 or ×0.01),
 * and must be provided in SI units.
 *
 * @param controller_id ID of the propulsion controller node (valid range:
 * 0–3). This ID is used to compute the CAN ID as `0x038 + controller_id`.
 * @param status Pointer to a filled `CixiCanData` structure containing the
 * values to send.
 * @return CIXI_CAN_OK if the frame was successfully transmitted;
 * CIXI_CAN_ERROR otherwise.
 */
CixiCanStatus cixi_can_send_status(uint8_t            controller_id,
                                   const CixiCanData *status);

/**
 * @brief Generate and return a populated CIXI status data structure.
 *
 * This function creates and fills a `CixiCanData` structure with
 * representative values for testing, simulation, or demonstration purposes.
 * The values are encoded in engineering units (e.g., Nm, A, V) and are
 * suitable for transmission over the CAN bus using `cixi_can_send_status`.
 *
 * All three status pages (0–2) can be generated from this single structure:
 * - Page 0: control value, motor torque, sensored RPM
 * - Page 1: electrical and mechanical power, senseless RPM
 * - Page 2: input voltage/current and Iq
 *
 * @return A `CixiCanData` struct containing example propulsion controller
 * data.
 */
CixiCanData cixi_get_status_data(CixiControllerState controller_state);

/**
 * @brief Processes a CIXI control command and sets motor behavior accordingly.
 *
 * - If ACTIVE and control_value ≠ 0: sets motor current.
 * - If ACTIVE and control_value == 0: applies brake current.
 * - If not ACTIVE: stops the motor.
 * Always resets timeout.
 *
 * @param[in] cmd Pointer to a CixiCanCommand.
 * @return true if handled successfully, false if input is NULL.
 */
bool handle_cixi_cmd(const CixiCanCommand *cmd);

void cixi_can_init(void);

void app_custom_start(void);
void app_custom_stop(void);

#endif // CAN_CIXI_COMMS_H
