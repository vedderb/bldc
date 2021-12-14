

#ifndef ENCODER_AS504X_H_
#define ENCODER_AS504X_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void AS504x_routine(void);
float AS504x_read_deg(void);
void AS504x_deinit(void);
encoders_ret_t AS504x_init(AS504x_config_t* AS504x_config);
AS504x_diag AS504x_get_diag(void);
bool AS504x_index_found(void);
float AS504x_spi_get_error_rate(void);
uint32_t AS504x_spi_get_val(void);
uint32_t AS504x_spi_get_error_cnt(void);

#define AS504x_SPI_READ_BIT 								0x4000
#define AS504x_SPI_WRITE_BIT 								0x0000

#define AS504x_SPI_DIAG_OCF_BIT_POS							8
#define AS504x_SPI_DIAG_COF_BIT_POS							9
#define AS504x_SPI_DIAG_COMP_LOW_BIT_POS					10
#define AS504x_SPI_DIAG_COMP_HIGH_BIT_POS					11

#define AS5047_SAMPLE_RATE_HZ		20000


#define AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK			0x3FFF


#define AS504x_SPI_DIAG_ADR									0x3FFD
#define AS504x_SPI_MAGN_ADR									0x3FFE
#define AS504x_SPI_CLEAR_ERROR_ADR							0x0001

#define AS504x_SPI_READ_DIAG_MSG							(AS504x_SPI_DIAG_ADR | AS504x_SPI_READ_BIT)
#define AS504x_SPI_READ_MAGN_MSG							(AS504x_SPI_MAGN_ADR | AS504x_SPI_READ_BIT)
#define AS504x_SPI_READ_CLEAR_ERROR_MSG						(AS504x_SPI_CLEAR_ERROR_ADR | AS504x_SPI_READ_BIT)

#define AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD		5

#define AS504x_DATA_INVALID_THRESHOLD						20000
#define AS504x_REFRESH_DIAG_AFTER_NSAMPLES					100

#endif /* ENCODER_AS504X_H_ */
