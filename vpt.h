#ifndef VPT_H_
#define VPT_H_

#include <stdint.h>

#define A_TO_CA 100.0
#define DUTY_SCALE_FACTOR 30000.0
#define SPEED_SCALE_FACTOR 0.5
#define VOLTS_TO_MILLIVOLTS 1000.0
#define ESC_TEMP_SCALE_FACTOR 2.0

typedef struct {
    uint16_t speed_half_rpm;
    int16_t current_cA;
    int16_t duty;
    uint16_t voltage_mV;
    uint8_t fault_code;
    uint8_t mc_state;
    uint8_t esc_temp;
} Telemetry;

#define ACK 0xAC
#define VESC_UART_HEADER_LEN 2
#define VESC_UART_TELEMETRY_LEN (VESC_UART_HEADER_LEN + 11)

typedef struct {
    uint8_t data[VESC_UART_TELEMETRY_LEN];
    unsigned int len;
} TxBuffer;

void vpt_process_cmd(uint8_t* rx_data, unsigned int rx_len,
                     void(*reply_func)(uint8_t* tx_data, unsigned int tx_len));

#endif
