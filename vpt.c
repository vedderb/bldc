#include <math.h>
#include <string.h>

#include "app.h"
#include "datatypes.h"
#include "mc_interface.h"
#include "timeout.h"
#include "vpt.h"

static void add_byte(TxBuffer* buffer, uint8_t byte)
{
    if (buffer->len < VESC_UART_TELEMETRY_LEN) {
        buffer->data[buffer->len++] = byte;
    }
}

static void set_duty(uint8_t* data, unsigned int len)
{
    int16_t duty;
    if (len >= sizeof(duty)) {
        duty = data[0] << 8 | data[1];
        mc_interface_set_duty(duty / DUTY_SCALE_FACTOR);
    }
}

static void set_speed(uint8_t* data, unsigned int len)
{
    int16_t speed;
    if (len >= sizeof(speed)) {
        speed = data[0] << 8 | data[1];
        mc_interface_set_pid_speed(speed / SPEED_SCALE_FACTOR);
    }
}

static void get_telemetry(TxBuffer* buffer)
{
    Telemetry tm;
    tm.speed_half_rpm = round(mc_interface_get_rpm() * SPEED_SCALE_FACTOR);
    tm.current_cA = round(mc_interface_get_tot_current_filtered() * A_TO_CA);
    tm.duty = round(mc_interface_get_duty_cycle_now() * DUTY_SCALE_FACTOR);
    tm.voltage_mV = round(GET_INPUT_VOLTAGE() * VOLTS_TO_MILLIVOLTS);
    tm.esc_temp = round(mc_interface_temp_fet_filtered() * ESC_TEMP_SCALE_FACTOR);
    tm.fault_code = mc_interface_get_fault();
    tm.mc_state = mc_interface_get_state();

    add_byte(buffer, tm.speed_half_rpm >> 8);
    add_byte(buffer, tm.speed_half_rpm & 0xFF);
    add_byte(buffer, tm.current_cA >> 8);
    add_byte(buffer, tm.current_cA & 0xFF);
    add_byte(buffer, tm.duty >> 8);
    add_byte(buffer, tm.duty & 0xFF);
    add_byte(buffer, tm.voltage_mV >> 8);
    add_byte(buffer, tm.voltage_mV & 0xFF);
    add_byte(buffer, tm.esc_temp);
    add_byte(buffer, tm.fault_code);
    add_byte(buffer, tm.mc_state);
}

void vpt_process_cmd(uint8_t* rx_data, unsigned int rx_len,
                     void(*reply_func)(uint8_t* tx_data, unsigned int tx_len))
{
    COMM_PACKET_ID packet_id = rx_data[0];
    uint8_t controller_id = app_get_configuration()->controller_id;
    if (rx_data[1] != controller_id) {
        return;
    }
    rx_data += 2;
    rx_len -= 2;

    TxBuffer buffer;
    buffer.len = 0;
    add_byte(&buffer, ACK);
    add_byte(&buffer, controller_id);

    switch (packet_id) {
    case COMM_VPT_PING:
        break;
    case COMM_VPT_SET_DUTY_GET_TELEMETRY:
        get_telemetry(&buffer);
        // fall through
    case COMM_VPT_SET_DUTY:
        set_duty(rx_data, rx_len);
        break;
    case COMM_VPT_SET_SPEED_GET_TELEMETRY:
        get_telemetry(&buffer);
        // fall through
    case COMM_VPT_SET_SPEED:
        set_speed(rx_data, rx_len);
        break;
    case COMM_VPT_GET_TELEMETRY:
        get_telemetry(&buffer);
        break;
    default:
        break;
    }

    reply_func(buffer.data, buffer.len);
    timeout_reset();
}
