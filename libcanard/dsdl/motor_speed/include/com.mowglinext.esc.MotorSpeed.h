#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <canard.h>


#define COM_MOWGLINEXT_ESC_MOTORSPEED_MAX_SIZE 6
#define COM_MOWGLINEXT_ESC_MOTORSPEED_SIGNATURE (0x4632879FFAABFF15ULL)
#define COM_MOWGLINEXT_ESC_MOTORSPEED_ID 20990

#if defined(__cplusplus) && defined(DRONECAN_CXX_WRAPPERS)
class com_mowglinext_esc_MotorSpeed_cxx_iface;
#endif

struct com_mowglinext_esc_MotorSpeed {
#if defined(__cplusplus) && defined(DRONECAN_CXX_WRAPPERS)
    using cxx_iface = com_mowglinext_esc_MotorSpeed_cxx_iface;
#endif
    uint8_t esc_index;
    bool valid;
    uint8_t sample_seq;
    float rpm;
};

#ifdef __cplusplus
extern "C"
{
#endif

uint32_t _com_mowglinext_esc_MotorSpeed_encode(struct com_mowglinext_esc_MotorSpeed* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
);
bool _com_mowglinext_esc_MotorSpeed_decode(const CanardRxTransfer* transfer, struct com_mowglinext_esc_MotorSpeed* msg);

static inline uint32_t com_mowglinext_esc_MotorSpeed_encode(struct com_mowglinext_esc_MotorSpeed* msg, uint8_t* buffer
#if CANARD_ENABLE_TAO_OPTION
    , bool tao
#endif
) {

    return _com_mowglinext_esc_MotorSpeed_encode(msg, buffer
#if CANARD_ENABLE_TAO_OPTION
    , tao
#endif
    );

}

static inline bool com_mowglinext_esc_MotorSpeed_decode(const CanardRxTransfer* transfer, struct com_mowglinext_esc_MotorSpeed* msg) {

    return _com_mowglinext_esc_MotorSpeed_decode(transfer, msg);

}

#if defined(CANARD_DSDLC_INTERNAL)
static inline void __com_mowglinext_esc_MotorSpeed_encode(uint8_t* buffer, uint32_t* bit_ofs, struct com_mowglinext_esc_MotorSpeed* msg, bool tao);
static inline bool __com_mowglinext_esc_MotorSpeed_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct com_mowglinext_esc_MotorSpeed* msg, bool tao);
void __com_mowglinext_esc_MotorSpeed_encode(uint8_t* buffer, uint32_t* bit_ofs, struct com_mowglinext_esc_MotorSpeed* msg, bool tao) {
    (void)buffer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;

    canardEncodeScalar(buffer, *bit_ofs, 5, &msg->esc_index);
    *bit_ofs += 5;
    canardEncodeScalar(buffer, *bit_ofs, 1, &msg->valid);
    *bit_ofs += 1;
    canardEncodeScalar(buffer, *bit_ofs, 8, &msg->sample_seq);
    *bit_ofs += 8;
    canardEncodeScalar(buffer, *bit_ofs, 32, &msg->rpm);
    *bit_ofs += 32;
}

/*
 decode com_mowglinext_esc_MotorSpeed, return true on failure, false on success
*/
bool __com_mowglinext_esc_MotorSpeed_decode(const CanardRxTransfer* transfer, uint32_t* bit_ofs, struct com_mowglinext_esc_MotorSpeed* msg, bool tao) {
    (void)transfer;
    (void)bit_ofs;
    (void)msg;
    (void)tao;
    canardDecodeScalar(transfer, *bit_ofs, 5, false, &msg->esc_index);
    *bit_ofs += 5;

    canardDecodeScalar(transfer, *bit_ofs, 1, false, &msg->valid);
    *bit_ofs += 1;

    canardDecodeScalar(transfer, *bit_ofs, 8, false, &msg->sample_seq);
    *bit_ofs += 8;

    canardDecodeScalar(transfer, *bit_ofs, 32, true, &msg->rpm);
    *bit_ofs += 32;

    return false; /* success */
}
#endif
#ifdef CANARD_DSDLC_TEST_BUILD
struct com_mowglinext_esc_MotorSpeed sample_com_mowglinext_esc_MotorSpeed_msg(void);
#endif
#ifdef __cplusplus
} // extern "C"

#ifdef DRONECAN_CXX_WRAPPERS
#include <canard/cxx_wrappers.h>
BROADCAST_MESSAGE_CXX_IFACE(com_mowglinext_esc_MotorSpeed, COM_MOWGLINEXT_ESC_MOTORSPEED_ID, COM_MOWGLINEXT_ESC_MOTORSPEED_SIGNATURE, COM_MOWGLINEXT_ESC_MOTORSPEED_MAX_SIZE);
#endif
#endif
