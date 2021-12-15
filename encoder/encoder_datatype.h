

#ifndef ENCODER_ENCODER_DATATYPE_H_
#define ENCODER_ENCODER_DATATYPE_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"

typedef uint8_t encoders_pin_t;
typedef uint32_t encoders_refresh_rate_hz_t;

typedef enum{
	ENCODERS_OK = 0,
	ENCODERS_NONE,
	ENCODERS_ERROR
}encoders_ret_t;

typedef enum{
	ENCODERS_TYPE_NONE = 0,
	ENCODERS_TYPE_AS504x,
	ENCODERS_TYPE_MT6816,
	ENCODERS_TYPE_AD2S1205_SPI,
	ENCODERS_TYPE_SINCOS,
	ENCODERS_TYPE_TS5700N8501,
	ENCODERS_TYPE_ABI
}encoders_type_t;

typedef struct{
	stm32_gpio_t* port;
	encoders_pin_t pin;
}encoders_gpio_t;

typedef struct{
	encoders_gpio_t gpio_nss;
	encoders_gpio_t gpio_miso;
	encoders_gpio_t gpio_mosi;
	encoders_gpio_t gpio_sck;
}encoders_spi_config_t;

typedef struct{
	encoders_gpio_t gpio_A;
	encoders_gpio_t gpio_B;
}encoders_incremental_config_t;

typedef struct {
	bool is_init;
	encoders_refresh_rate_hz_t refresh_rate_hz; //TODO: REWRITE TO POINTER OF AS504x_config_t
	encoders_spi_config_t spi_config;	//TODO: REWRITE TO POINTER OF AS504x_config_t
}AS504x_config_t;

typedef struct {
	bool is_init;
	encoders_refresh_rate_hz_t refresh_rate_hz; //TODO: REWRITE TO POINTER OF AS504x_config_t
	encoders_spi_config_t spi_config;	//TODO: REWRITE TO POINTER OF AS504x_config_t
}MT6816_config_t;

typedef struct {
	bool is_init;
	encoders_refresh_rate_hz_t refresh_rate_hz;
	encoders_spi_config_t spi_config;
}AD2S1205_config_t;

typedef struct {
	bool is_init;
	uint32_t counts;
	encoders_incremental_config_t incremental_config;
}ABI_config_t;

typedef struct{
	encoders_type_t encoder_type;
	encoders_refresh_rate_hz_t refresh_rate_hz;
	encoders_spi_config_t spi_config;
	uint32_t counts; // FOR INCREMENTAL INTERFACE
	encoders_incremental_config_t incremental_config; // FOR INCREMENTAL INTERFACE
}encoders_config_t;



#endif /* ENCODER_ENCODER_DATATYPE_H_ */
