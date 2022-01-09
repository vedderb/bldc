#ifndef ENCODER_ENCODER_DATATYPE_H_
#define ENCODER_ENCODER_DATATYPE_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"

typedef uint8_t encoder_pin_t;
typedef uint32_t encoder_refresh_rate_hz_t;

typedef enum {
	ENCODER_OK = 0, ENCODER_NONE, ENCODER_ERROR
} encoder_ret_t;

typedef enum {
	ENCODER_TYPE_NONE = 0,
	ENCODER_TYPE_AS504x,
	ENCODER_TYPE_MT6816,
	ENCODER_TYPE_AD2S1205_SPI,
	ENCODER_TYPE_SINCOS,
	ENCODER_TYPE_TS5700N8501,
	ENCODER_TYPE_ABI
} encoder_type_t;

typedef struct {
	stm32_gpio_t *port;
	encoder_pin_t pin;
} encoder_gpio_t;

typedef struct {
	encoder_gpio_t gpio_nss;
	encoder_gpio_t gpio_miso;
	encoder_gpio_t gpio_mosi;
	encoder_gpio_t gpio_sck;
} encoder_spi_config_t;

typedef struct {
	encoder_gpio_t gpio_A;
	encoder_gpio_t gpio_B;
} encoder_incremental_config_t;

typedef struct {
	encoder_gpio_t gpio_TX;
	encoder_gpio_t gpio_RX;
	encoder_gpio_t gpio_EXT;
} encoder_UART_config_t;

typedef struct {
	bool is_init;
	encoder_refresh_rate_hz_t refresh_rate_hz;
	encoder_spi_config_t spi_config;
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
	encoder_incremental_config_t incremental_config;
} ABI_config_t;

typedef struct {
	bool is_init;
	encoder_refresh_rate_hz_t refresh_rate_hz;
	float s_gain;
	float s_offset;
	float c_gain;
	float c_offset;
	float filter_constant;
} ENCSINCOS_config_t;

typedef struct {
	bool is_init;
	encoder_UART_config_t uart_config;
	SerialConfig uart_param;
} TS5700N8501_config_t;
#endif /* ENCODER_ENCODER_DATATYPE_H_ */
