#define CANARD_DSDLC_INTERNAL
#include <com.mowglinext.esc.MotorSpeed.h>
#include <string.h>

#ifdef CANARD_DSDLC_TEST_BUILD
#include <test_helpers.h>
#endif

uint32_t _com_mowglinext_esc_MotorSpeed_encode(struct com_mowglinext_esc_MotorSpeed* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
) {
    uint32_t bit_ofs = 0;
    memset(buffer, 0, COM_MOWGLINEXT_ESC_MOTORSPEED_MAX_SIZE);
    __com_mowglinext_esc_MotorSpeed_encode(buffer, &bit_ofs, msg,
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
bool _com_mowglinext_esc_MotorSpeed_decode(const CanardRxTransfer* transfer, struct com_mowglinext_esc_MotorSpeed* msg) {
#if CANARD_ENABLE_TAO_OPTION
    if (transfer->tao && (transfer->payload_len > COM_MOWGLINEXT_ESC_MOTORSPEED_MAX_SIZE)) {
        return true; /* invalid payload length */
    }
#endif
    uint32_t bit_ofs = 0;
    if (__com_mowglinext_esc_MotorSpeed_decode(transfer, &bit_ofs, msg,
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
struct com_mowglinext_esc_MotorSpeed sample_com_mowglinext_esc_MotorSpeed_msg(void) {
    struct com_mowglinext_esc_MotorSpeed msg;

    msg.esc_index = (uint8_t)random_bitlen_unsigned_val(5);
    msg.valid = (bool)random_bitlen_unsigned_val(1);
    msg.sample_seq = (uint8_t)random_bitlen_unsigned_val(8);
    msg.rpm = random_float_val();
    return msg;
}
#endif
