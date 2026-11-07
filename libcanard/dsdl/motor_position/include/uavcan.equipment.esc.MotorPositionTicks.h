#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <canard.h>


#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONTICKS_MAX_SIZE 6
#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONTICKS_SIGNATURE (0x9199C4D9F05D2B4BULL)


struct uavcan_equipment_esc_MotorPositionTicks {
    int32_t ticks;
    uint16_t ticks_per_revolution;
};

#ifdef __cplusplus
extern "C"
{
#endif

uint32_t _uavcan_equipment_esc_MotorPositionTicks_encode(struct uavcan_equipment_esc_MotorPositionTicks* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
);
bool _uavcan_equipment_esc_MotorPositionTicks_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPositionTicks* msg);

static inline uint32_t uavcan_equipment_esc_MotorPositionTicks_encode(struct uavcan_equipment_esc_MotorPositionTicks* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
) {

    return _uavcan_equipment_esc_MotorPositionTicks_encode(msg, buffer
#if CANARD_ENABLE_TAO_OPTION
    , tao
#endif
    );

}

static inline bool uavcan_equipment_esc_MotorPositionTicks_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPositionTicks* msg) {

    return _uavcan_equipment_esc_MotorPositionTicks_decode(transfer, msg);

}

#if defined(CANARD_DSDLC_INTERNAL)
static inline void __uavcan_equipment_esc_MotorPositionTicks_encode(uint8_t* buffer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionTicks* msg, bool tao);
static inline bool __uavcan_equipment_esc_MotorPositionTicks_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionTicks* msg, bool tao);
void __uavcan_equipment_esc_MotorPositionTicks_encode(uint8_t* buffer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionTicks* msg, bool tao) {
    (void)buffer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;

    canardEncodeScalar(buffer, *bit_ofs, 32, &msg->ticks);
    *bit_ofs += 32;
    canardEncodeScalar(buffer, *bit_ofs, 16, &msg->ticks_per_revolution);
    *bit_ofs += 16;
}

/*
 decode uavcan_equipment_esc_MotorPositionTicks, return true on failure, false on success
*/
bool __uavcan_equipment_esc_MotorPositionTicks_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionTicks* msg, bool tao) {
    (void)transfer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;
    canardDecodeScalar(transfer, *bit_ofs, 32, true, &msg->ticks);
    *bit_ofs += 32;

    canardDecodeScalar(transfer, *bit_ofs, 16, false, &msg->ticks_per_revolution);
    *bit_ofs += 16;

    return false; /* success */
}
#endif
#ifdef CANARD_DSDLC_TEST_BUILD
struct uavcan_equipment_esc_MotorPositionTicks sample_uavcan_equipment_esc_MotorPositionTicks_msg(void);
#endif
#ifdef __cplusplus
} // extern "C"

#ifdef DRONECAN_CXX_WRAPPERS
#include <canard/cxx_wrappers.h>
#endif
#endif
