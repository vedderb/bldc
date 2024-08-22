/*
	Copyright 2022 Jakub Tomczak
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ENCODER_DATATYPE_H_
#define ENCODER_DATATYPE_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"
#include "spi_bb.h"

typedef enum {
	ENCODER_TYPE_NONE = 0,
	ENCODER_TYPE_AS504x,
	ENCODER_TYPE_MT6816,
	ENCODER_TYPE_TLE5012,
	ENCODER_TYPE_AD2S1205_SPI,
	ENCODER_TYPE_SINCOS,
	ENCODER_TYPE_TS5700N8501,
	ENCODER_TYPE_ABI,
	ENCODER_TYPE_AS5x47U,
	ENCODER_TYPE_BISSC,
	ENCODER_TYPE_CUSTOM
} encoder_type_t;

typedef struct {
	uint16_t spi_val;
	float resolver_loss_of_tracking_error_rate;
	float resolver_degradation_of_signal_error_rate;
	float resolver_loss_of_signal_error_rate;
	uint32_t resolver_loss_of_tracking_error_cnt;
	uint32_t resolver_degradation_of_signal_error_cnt;
	uint32_t resolver_loss_of_signal_error_cnt;
	uint32_t spi_error_cnt;
	float spi_error_rate;
	float last_enc_angle;
	uint32_t last_update_time;
	uint32_t resolver_void_packet_cnt;
	uint32_t resolver_vel_packet_cnt;
	float resolver_void_packet_error_rate;
	float resolver_vel_packet_error_rate;
	float resolver_LOT_peak_error_rate;
	float resolver_LOS_peak_error_rate;
	float resolver_DOS_peak_error_rate;
	float resolver_SPI_peak_error_rate;
	float resolver_VELread_peak_error_rate;
	float resolver_VOIDspi_peak_error_rate;
} AD2S1205_state;

typedef struct {
	spi_bb_state sw_spi;
	AD2S1205_state state;
} AD2S1205_config_t;

typedef struct {
	float spi_error_rate;
	float encoder_no_magnet_error_rate;
	uint32_t encoder_no_magnet_error_cnt;
	float last_enc_angle;
	uint32_t spi_error_cnt;
	uint32_t spi_val;
	uint32_t last_update_time;
} MT6816_state;

typedef struct {
	SPIDriver *spi_dev;
	SPIConfig hw_spi_cfg;
	uint8_t spi_af;
	stm32_gpio_t *nss_gpio;
	int nss_pin;
	stm32_gpio_t *sck_gpio;
	int sck_pin;
	stm32_gpio_t *mosi_gpio;
	int mosi_pin;
	stm32_gpio_t *miso_gpio;
	int miso_pin;

	MT6816_state state;
} MT6816_config_t;

typedef struct {
	float spi_error_rate;
	float encoder_no_magnet_error_rate;
	uint32_t encoder_no_magnet_error_cnt;
	float last_enc_angle;
	uint32_t spi_error_cnt;
	uint8_t last_status_error;
	uint32_t spi_val;
	uint32_t last_update_time;
} TLE5012_state;

typedef struct { // sw ssc
	spi_bb_state sw_spi;
	TLE5012_state state;
} TLE5012_config_t;

typedef enum tle5012_errortypes {
	NO_ERROR               = 0x00,  //!< NO_ERROR = Safety word was OK
	SYSTEM_ERROR           = 0x01,  //!< SYSTEM_ERROR = over/under voltage, VDD negative, GND off, ROM defect, no magnet
	INTERFACE_ACCESS_ERROR = 0x02,  //!< INTERFACE_ACCESS_ERROR = wrong address or wrong lock
	INVALID_ANGLE_ERROR    = 0x04,  //!< INVALID_ANGLE_ERROR = NO_GMR_A = 1 or NO_GMR_XY = 1
	ANGLE_SPEED_ERROR      = 0x08,  //!< ANGLE_SPEED_ERROR = combined error, angular speed calculation wrong
	CRC_ERROR              = 0xFF   //!< CRC_ERROR = Cyclic Redundancy Check (CRC), which includes the STAT and RESP bits wrong
} tle5012_errortypes; 

typedef struct {
	volatile bool index_found;
	volatile int bad_pulses;
} ABI_state;

typedef struct {
	uint32_t counts;

	stm32_gpio_t *A_gpio; uint8_t A_pin;
	stm32_gpio_t *B_gpio; uint8_t B_pin;
	stm32_gpio_t *I_gpio; uint8_t I_pin;

	TIM_TypeDef *timer;
	uint8_t tim_af;

	uint8_t exti_portsrc;
	uint8_t exti_pinsrc;
	uint32_t exti_line;
	uint32_t exti_ch;

	ABI_state state;
} ABI_config_t;

typedef struct {
	uint32_t signal_below_min_error_cnt;
	uint32_t signal_above_max_error_cnt;
	float signal_low_error_rate;
	float signal_above_max_error_rate;
	float last_enc_angle;
	float sin_filter;
	float cos_filter;
	uint32_t last_update_time;
} ENCSINCOS_state;

typedef struct {
	uint32_t refresh_rate_hz;
	// The gain is 1/amplutide. The reason it is stored like that
	// is to avoid two divisions when reading the encoder.
	float s_gain;
	float c_gain;
	float s_offset;
	float c_offset;
	float filter_constant;
	float phase_correction; //phase angle correction (in deg) when encoder outputs sin(anle)/cos(angle+pase_correction)
	float sph; // sin of the phase_correction angle
	float cph; // cos of the phase_correction angle

	ENCSINCOS_state state;
} ENCSINCOS_config_t;

typedef struct {
	volatile bool stop_now;
	volatile bool is_running;
	volatile uint8_t raw_status[8];
	volatile bool reset_errors;
	volatile bool reset_multiturn;
	float spi_error_rate;
	uint32_t spi_error_cnt;
	uint32_t spi_val;
	float last_enc_angle;
} TS5700N8501_state;

typedef struct {
	SerialDriver *sd;
	uint8_t sd_af;
	stm32_gpio_t *TX_gpio; uint8_t TX_pin;
	stm32_gpio_t *RX_gpio; uint8_t RX_pin;
	stm32_gpio_t *EXT_gpio; uint8_t EXT_pin;
	SerialConfig uart_param;
	stkalign_t *thread_wa;
	uint32_t thread_wa_size;

	TS5700N8501_state state;
} TS5700N8501_config_t;

typedef struct {
	uint8_t is_connected;
	uint8_t AGC_value;
	uint16_t magnitude;
	uint8_t is_OCF;
	uint8_t is_COF;
	uint8_t is_Comp_low;
	uint8_t is_Comp_high;
	uint16_t serial_diag_flgs;
	uint16_t serial_magnitude;
	uint16_t serial_error_flags;
} AS504x_diag;

typedef struct {
	uint16_t diag_fetch_now_count;
	uint32_t data_last_invalid_counter;
	uint32_t spi_communication_error_count;
	uint8_t spi_data_err_raised;
	AS504x_diag sensor_diag;
	uint16_t spi_val;
	float last_enc_angle;
	uint32_t spi_error_cnt;
	float spi_error_rate;
	uint32_t last_update_time;
} AS504x_state;

typedef struct {
	spi_bb_state sw_spi;
	AS504x_state state;
} AS504x_config_t;

typedef struct {
	uint8_t is_connected;
	uint8_t is_broken_hall;
	uint8_t is_error;
	uint8_t is_COF;
	uint8_t is_Comp_low;
	uint8_t is_Comp_high;
	uint8_t is_wdtst;
	uint8_t is_crc_error;
	uint8_t is_mag_half;
	uint8_t AGC_value;
	uint16_t magnitude;
	uint16_t serial_AGC_value;
	uint16_t serial_diag_flgs;
	uint16_t serial_magnitude;
	uint16_t serial_error_flgs;
} AS5x47U_diag;

typedef struct {
	uint16_t spi_seq;
	uint32_t spi_communication_error_count;
	AS5x47U_diag sensor_diag;
	uint16_t spi_val;
	float last_enc_angle;
	uint32_t spi_error_cnt;
	float spi_error_rate;
	uint32_t last_update_time;
	uint8_t rx_buf[4];
	uint8_t tx_buf[4];
} AS5x47U_state;

typedef struct {
	SPIDriver *spi_dev;
	SPIConfig hw_spi_cfg;
	uint8_t spi_af;
	stm32_gpio_t *nss_gpio;
	int nss_pin;
	stm32_gpio_t *sck_gpio;
	int sck_pin;
	stm32_gpio_t *mosi_gpio;
	int mosi_pin;
	stm32_gpio_t *miso_gpio;
	int miso_pin;
	AS5x47U_state state;
} AS5x47U_config_t;

typedef struct {
	float spi_data_error_rate;
	uint32_t spi_data_error_cnt;
	float spi_comm_error_rate;
	uint32_t spi_comm_error_cnt;
	float last_enc_angle;
	uint32_t spi_val;
	uint32_t last_update_time;
	uint8_t decod_buf[8];
} BISSC_state;

typedef struct {
	SPIDriver *spi_dev;
	SPIConfig hw_spi_cfg;
	uint8_t spi_af;
	stm32_gpio_t *nss_gpio;
	int nss_pin;
	stm32_gpio_t *sck_gpio;
	int sck_pin;
	stm32_gpio_t *mosi_gpio;
	int mosi_pin;
	stm32_gpio_t *miso_gpio;
	int miso_pin;

	uint32_t enc_res;
	uint8_t tableCRC6n[64];

	BISSC_state state;
} BISSC_config_t;

#endif /* ENCODER_DATATYPE_H_ */
