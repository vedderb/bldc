#ifndef ENCODER_ENCODER_DATATYPE_H_
#define ENCODER_ENCODER_DATATYPE_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"

#define AS5047_SAMPLE_RATE_HZ       20000
#define AD2S1205_SAMPLE_RATE_HZ     20000       //25MHz max spi clk
#define TS5700N8501_SAMPLE_RATE_HZ	20000
#define MT6816_SAMPLE_RATE_HZ       20000
#define MT6816_NO_MAGNET_ERROR_MASK 0x0002
#define SINCOS_SAMPLE_RATE_HZ       20000
#define SINCOS_MIN_AMPLITUDE        1.0         // sqrt(sin^2 + cos^2) has to be larger than this
#define SINCOS_MAX_AMPLITUDE        1.65        // sqrt(sin^2 + cos^2) has to be smaller than this

typedef uint8_t encoders_pin_t;
typedef uint32_t encoders_refresh_rate_hz_t;

typedef enum {
	ENCODERS_OK = 0, ENCODERS_NONE, ENCODERS_ERROR
} encoders_ret_t;

typedef enum {
	ENCODERS_TYPE_NONE = 0,
	ENCODERS_TYPE_AS504x,
	ENCODERS_TYPE_MT6816,
	ENCODERS_TYPE_AD2S1205_SPI,
	ENCODERS_TYPE_SINCOS,
	ENCODERS_TYPE_TS5700N8501,
	ENCODERS_TYPE_ABI
} encoders_type_t;

typedef struct {
	stm32_gpio_t *port;
	encoders_pin_t pin;
} encoders_gpio_t;

typedef struct {
	encoders_gpio_t gpio_nss;
	encoders_gpio_t gpio_miso;
	encoders_gpio_t gpio_mosi;
	encoders_gpio_t gpio_sck;
} encoders_spi_config_t;

typedef struct {
	encoders_gpio_t gpio_A;
	encoders_gpio_t gpio_B;
} encoders_incremental_config_t;

typedef struct {
	encoders_gpio_t gpio_TX;
	encoders_gpio_t gpio_RX;
	encoders_gpio_t gpio_EXT;
} encoders_UART_config_t;

typedef struct {
	bool is_init;
	encoders_refresh_rate_hz_t refresh_rate_hz;
	encoders_spi_config_t spi_config;
#ifdef HW_SPI_DEV
	SPIConfig hw_spi_cfg;
#endif
} ENCSPI_config_t;

typedef ENCSPI_config_t MT6816_config_t;
typedef ENCSPI_config_t AD2S1205_config_t;
typedef ENCSPI_config_t AS504x_config_t;

typedef struct {
	bool is_init;
	uint32_t counts;
	encoders_incremental_config_t incremental_config;
} ABI_config_t;

typedef struct {
	bool is_init;
	encoders_refresh_rate_hz_t refresh_rate_hz;
	float s_gain;
	float s_offset;
	float c_gain;
	float c_offset;
	float filter_constant;
} ENCSINCOS_config_t;

typedef struct {
	bool is_init;
	encoders_UART_config_t uart_config;
	SerialConfig uart_param;
} TS5700N8501_config_t;

typedef struct {
	encoders_type_t encoder_type;
	ENCSPI_config_t encspi;
	ABI_config_t abi;
	ENCSINCOS_config_t encsincos;
	TS5700N8501_config_t ts5700n8501;
} encoders_config_t;
/*
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
}AS504x_diag;
*/
#endif /* ENCODER_ENCODER_DATATYPE_H_ */
