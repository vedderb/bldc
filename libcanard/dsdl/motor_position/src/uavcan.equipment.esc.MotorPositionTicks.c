#define CANARD_DSDLC_INTERNAL
#include <uavcan.equipment.esc.MotorPositionTicks.h>
#include <string.h>

#ifdef CANARD_DSDLC_TEST_BUILD
#include <test_helpers.h>
#endif

uint32_t _uavcan_equipment_esc_MotorPositionTicks_encode(struct uavcan_equipment_esc_MotorPositionTicks* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
) {
    uint32_t bit_ofs = 0;
    memset(buffer, 0, UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONTICKS_MAX_SIZE);
    __uavcan_equipment_esc_MotorPositionTicks_encode(buffer, &bit_ofs, msg,
#if CANARD_ENABLE_TAO_OPTION
    tao
#else
    true
#endif
    );
    return ((bit_ofs+7)/8);
}

/*
  return true if the decode is invalid
 */
bool _uavcan_equipment_esc_MotorPositionTicks_decode(const CanardRxTransfer* transfer, struct uavcan_equipment_esc_MotorPositionTicks* msg) {
#if CANARD_ENABLE_TAO_OPTION
    if (transfer->tao && (transfer->payload_len > UAVCAN_EQUIPMENT_ESC_MOTORPOSITIONTICKS_MAX_SIZE)) {
        return true; /* invalid payload length */
    }
#endif
    uint32_t bit_ofs = 0;
    if (__uavcan_equipment_esc_MotorPositionTicks_decode(transfer, &bit_ofs, msg,
#if CANARD_ENABLE_TAO_OPTION
    transfer->tao
#else
    true
#endif
    )) {
        return true; /* invalid payload */
    }

    const uint32_t byte_len = (bit_ofs+7U)/8U;
#if CANARD_ENABLE_TAO_OPTION
    // if this could be CANFD then the dlc could indicating more bytes than
    // we actually have
    if (!transfer->tao) {
        return byte_len > transfer->payload_len;
    }
#endif
    return byte_len != transfer->payload_len;
}

#ifdef CANARD_DSDLC_TEST_BUILD
struct uavcan_equipment_esc_MotorPositionTicks sample_uavcan_equipment_esc_MotorPositionTicks_msg(void) {
    struct uavcan_equipment_esc_MotorPositionTicks msg;

    msg.ticks = (int32_t)random_bitlen_signed_val(32);
    msg.ticks_per_revolution = (uint16_t)random_bitlen_unsigned_val(16);
    return msg;
}
#endif
