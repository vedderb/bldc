#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <canard.h>
#include <uavcan.equipment.esc.MotorPositionTicks.h>


#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_MAX_SIZE 7
#define UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_SIGNATURE (0x50B78B06CA574C96ULL)

enum uavcan_equipment_esc_MotorPositionValue_type_t {
    UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_TICKS,
    UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_REVOLUTIONS,
};


struct uavcan_equipment_esc_MotorPositionValue {
    enum uavcan_equipment_esc_MotorPositionValue_type_t union_tag;
    union {
        struct uavcan_equipment_esc_MotorPositionTicks ticks;
        float revolutions;
    };
};

#ifdef __cplusplus
extern "C"
{
#endif

uint32_t _uavcan_equipment_esc_MotorPositionValue_encode(struct uavcan_equipment_esc_MotorPositionValue* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
);
bool _uavcan_equipment_esc_MotorPositionValue_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPositionValue* msg);

static inline uint32_t uavcan_equipment_esc_MotorPositionValue_encode(struct uavcan_equipment_esc_MotorPositionValue* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
) {

    return _uavcan_equipment_esc_MotorPositionValue_encode(msg, buffer
#if CANARD_ENABLE_TAO_OPTION
    , tao
#endif
    );

}

static inline bool uavcan_equipment_esc_MotorPositionValue_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPositionValue* msg) {

    return _uavcan_equipment_esc_MotorPositionValue_decode(transfer, msg);

}

#if defined(CANARD_DSDLC_INTERNAL)
static inline void __uavcan_equipment_esc_MotorPositionValue_encode(uint8_t* buffer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionValue* msg, bool tao);
static inline bool __uavcan_equipment_esc_MotorPositionValue_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionValue* msg, bool tao);
void __uavcan_equipment_esc_MotorPositionValue_encode(uint8_t* buffer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionValue* msg, bool tao) {
    (void)buffer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;

    uint8_t union_tag = msg->union_tag;
    canardEncodeScalar(buffer, *bit_ofs, 1, &union_tag);
    *bit_ofs += 1;

    switch(msg->union_tag) {
        case UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_TICKS: {
            __uavcan_equipment_esc_MotorPositionTicks_encode(buffer, bit_ofs, &msg->ticks, tao);
            break;
        }
        case UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_REVOLUTIONS: {
            canardEncodeScalar(buffer, *bit_ofs, 32, &msg->revolutions);
            *bit_ofs += 32;
            break;
        }
    }
}

/*
 decode uavcan_equipment_esc_MotorPositionValue, return true on failure, false on success
*/
bool __uavcan_equipment_esc_MotorPositionValue_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct uavcan_equipment_esc_MotorPositionValue* msg, bool tao) {
    (void)transfer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;
    uint8_t union_tag;
    canardDecodeScalar(transfer, *bit_ofs, 1, false, &union_tag);
    *bit_ofs += 1;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wtype-limits"
    if (union_tag >= 2) {
        return true; /* invalid value */
    }
#pragma GCC diagnostic pop
    msg->union_tag = (enum uavcan_equipment_esc_MotorPositionValue_type_t)union_tag;

    switch(msg->union_tag) {
        case UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_TICKS: {
            if (__uavcan_equipment_esc_MotorPositionTicks_decode(transfer, bit_ofs, &msg->ticks, tao)) {return true;}
            break;
        }

        case UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONVALUE_REVOLUTIONS: {
            canardDecodeScalar(transfer, *bit_ofs, 32, true, &msg->revolutions);
            *bit_ofs += 32;
            break;
        }

    }
    return false; /* success */
}
#endif
#ifdef CANARD_DSDLC_TEST_BUILD
struct uavcan_equipment_esc_MotorPositionValue sample_uavcan_equipment_esc_MotorPositionValue_msg(void);
#endif
#ifdef __cplusplus
} // extern "C"

#ifdef DRONECAN_CXX_WRAPPERS
#include <canard/cxx_wrappers.h>
#endif
#endif
