/**
 * Author Wojciech Domski <Wojciech.Domski@gmail.com>
 * www: www.Domski.pl
 *
 * Hardware layer for SX1278 LoRa module
 */

#ifndef __SX1278_HW_HEADER
#define __SX1278_HW_HEADER

#include <stdint.h>
#include "hal.h"

/**
 * \brief Initialize hardware layer
 *
 * Clears NSS and resets LoRa module.
 *
 */
void SX1278_hw_init(void);

/**
 * \brief Resets LoRa module
 *
 * Resets LoRa module.
 *
 */
void SX1278_hw_Reset(void);

/**
 * \brief Control NSS
 *
 * Clears and sets NSS according to passed value.
 *
 * \param[in]   value   1 sets NSS high, other value sets NSS low.
 */
void SX1278_hw_SetNSS(int value);


/**
 * \brief Send command via SPI.
 *
 * Send single byte via SPI interface.
 *
 * \param[in]   cmd		Command
 */
void SX1278_hw_SPICommand(uint8_t cmd);

/**
 * \brief Reads data via SPI
 *
 * Reads data via SPI interface.
 *
 * \return				Read value
 */
uint8_t SX1278_hw_SPIReadByte(void);

/**
 * \brief ms delay
 *
 * Milisecond delay.
 *
 * \param[in]   msec 		Number of milliseconds to wait
 */
void SX1278_hw_DelayMs(uint32_t msec);

/**
 * \brief Reads DIO0 state
 *
 * Reads LoRa DIO0 state using GPIO.
 *
 * \return				0 if DIO0 low, 1 if DIO high
 */
int SX1278_hw_GetDIO0(void);

#endif

