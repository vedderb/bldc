#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <canard.h>
#include <uavcan.equipment.esc.MotorPositionValue.h>


#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITION_MAX_SIZE 12
#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITION_SIGNATURE (0x424319C8A23C5E29ULL)
#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITION_ID 1037

#if defined(__cplusplus) && defined(DRONECAN_CXX_WRAPPERS)
class uavcan_equipment_esc_MotorPosition_cxx_iface;
#endif

struct uavcan_equipment_esc_MotorPosition {
#if defined(__cplusplus) && defined(DRONECAN_CXX_WRAPPERS)
    using cxx_iface = uavcan_equipment_esc_MotorPosition_cxx_iface;
#endif
    uint8_t esc_index;
    bool valid;
    uint32_t origin_id;
    uint8_t sample_seq;
    struct uavcan_equipment_esc_MotorPositionValue position;
};

#ifdef __cplusplus
extern "C"
{
#endif

uint32_t _uavcan_equipment_esc_MotorPosition_encode(struct uavcan_equipment_esc_MotorPosition* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
);
bool _uavcan_equipment_esc_MotorPosition_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPosition* msg);

static inline uint32_t uavcan_equipment_esc_MotorPosition_encode(struct uavcan_equipment_esc_MotorPosition* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
) {

    return _uavcan_equipment_esc_MotorPosition_encode(msg, buffer
#if CANARD_ENABLE_TAO_OPTION
    , tao
#endif
    );

}

static inline bool uavcan_equipment_esc_MotorPosition_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPosition* msg) {

    return _uavcan_equipment_esc_MotorPosition_decode(transfer, msg);

}

#if defined(CANARD_DSDLC_INTERNAL)
static inline void __uavcan_equipment_esc_MotorPosition_encode(uint8_t* buffer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPosition* msg, bool tao);
static inline bool __uavcan_equipment_esc_MotorPosition_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPosition* msg, bool tao);
void __uavcan_equipment_esc_MotorPosition_encode(uint8_t* buffer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPosition* msg, bool tao) {
    (void)buffer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;

    canardEncodeScalar(buffer, *bit_ofs, 5, &msg->esc_index);
    *bit_ofs += 5;
    canardEncodeScalar(buffer, *bit_ofs, 1, &msg->valid);
    *bit_ofs += 1;
    canardEncodeScalar(buffer, *bit_ofs, 32, &msg->origin_id);
    *bit_ofs += 32;
    canardEncodeScalar(buffer, *bit_ofs, 8, &msg->sample_seq);
    *bit_ofs += 8;
    __uavcan_equipment_esc_MotorPositionValue_encode(buffer, bit_ofs, &msg->position, tao);
}

/*
 decode uavcan_equipment_esc_MotorPosition, return true on failure, false on success
*/
bool __uavcan_equipment_esc_MotorPosition_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPosition* msg, bool tao) {
    (void)transfer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;
    canardDecodeScalar(transfer, *bit_ofs, 5, false, &msg->esc_index);
    *bit_ofs += 5;

    canardDecodeScalar(transfer, *bit_ofs, 1, false, &msg->valid);
    *bit_ofs += 1;

    canardDecodeScalar(transfer, *bit_ofs, 32, false, &msg->origin_id);
    *bit_ofs += 32;

    canardDecodeScalar(transfer, *bit_ofs, 8, false, &msg->sample_seq);
    *bit_ofs += 8;

    if (__uavcan_equipment_esc_MotorPositionValue_decode(transfer, bit_ofs, &msg->position, tao)) {return true;}

    return false; /* success */
}
#endif
#ifdef CANARD_DSDLC_TEST_BUILD
struct uavcan_equipment_esc_MotorPosition sample_uavcan_equipment_esc_MotorPosition_msg(void);
#endif
#ifdef __cplusplus
} // extern "C"

#ifdef DRONECAN_CXX_WRAPPERS
#include <canard/cxx_wrappers.h>
BROADCAST_MESSAGE_CXX_IFACE(uavcan_equipment_esc_MotorPosition, UAVCAN_EQUIPMENT_ESC_MOTORPOSITION_ID, UAVCAN_EQUIPMENT_ESC_MOTORPOSITION_SIGNATURE, UAVCAN_EQUIPMENT_ESC_MOTORPOSITION_MAX_SIZE);
#endif
#endif
