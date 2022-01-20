
#ifndef ENCODER_ENCODER_DATATYPE_H_
#define ENCODER_ENCODER_DATATYPE_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"
#include "spi_bb.h"

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
	bool is_init;
	uint32_t refresh_rate_hz;
	spi_bb_state sw_spi;
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
	stm32_gpio_t *port_A;
	uint8_t pin_A;
	stm32_gpio_t *port_B;
	uint8_t pin_B;
} ABI_config_t;

typedef struct {
	bool is_init;
	uint32_t refresh_rate_hz;
	float s_gain;
	float s_offset;
	float c_gain;
	float c_offset;
	float filter_constant;
} ENCSINCOS_config_t;

typedef struct {
	bool is_init;
	stm32_gpio_t *port_TX;
	uint8_t pin_TX;
	stm32_gpio_t *port_RX;
	uint8_t pin_RX;
	stm32_gpio_t *port_EXT;
	uint8_t pin_EXT;
	SerialConfig uart_param;
} TS5700N8501_config_t;
#endif /* ENCODER_ENCODER_DATATYPE_H_ */
