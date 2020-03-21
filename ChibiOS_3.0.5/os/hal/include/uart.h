/*
    ChibiOS - Copyright (C) 2006..2015 Giovanni Di Sirio

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

/**
 * @file    uart.h
 * @brief   UART Driver macros and structures.
 *
 * @addtogroup UART
 * @{
 */

#ifndef _UART_H_
#define _UART_H_

#if (HAL_USE_UART == TRUE) || defined(__DOXYGEN__)

/*===========================================================================*/
/* Driver constants.                                                         */
/*===========================================================================*/

/**
 * @name    UART status flags
 * @{
 */
#define UART_NO_ERROR           0   /**< @brief No pending conditions.      */
#define UART_PARITY_ERROR       4   /**< @brief Parity error happened.      */
#define UART_FRAMING_ERROR      8   /**< @brief Framing error happened.     */
#define UART_OVERRUN_ERROR      16  /**< @brief Overflow happened.          */
#define UART_NOISE_ERROR        32  /**< @brief Noise on the line.          */
#define UART_BREAK_DETECTED     64  /**< @brief Break detected.             */
/** @} */

/*===========================================================================*/
/* Driver pre-compile time settings.                                         */
/*===========================================================================*/

/*===========================================================================*/
/* Derived constants and error checks.                                       */
/*===========================================================================*/

/*===========================================================================*/
/* Driver data structures and types.                                         */
/*===========================================================================*/

/**
 * @brief   Driver state machine possible states.
 */
typedef enum {
  UART_UNINIT = 0,                  /**< Not initialized.                   */
  UART_STOP = 1,                    /**< Stopped.                           */
  UART_READY = 2                    /**< Ready.                             */
} uartstate_t;

/**
 * @brief   Transmitter state machine states.
 */
typedef enum {
  UART_TX_IDLE = 0,                 /**< Not transmitting.                  */
  UART_TX_ACTIVE = 1,               /**< Transmitting.                      */
  UART_TX_COMPLETE = 2              /**< Buffer complete.                   */
} uarttxstate_t;

/**
 * @brief   Receiver state machine states.
 */
typedef enum {
  UART_RX_IDLE = 0,                 /**< Not receiving.                     */
  UART_RX_ACTIVE = 1,               /**< Receiving.                         */
  UART_RX_COMPLETE = 2              /**< Buffer complete.                   */
} uartrxstate_t;

#include "uart_lld.h"

/*===========================================================================*/
/* Driver macros.                                                            */
/*===========================================================================*/

/*===========================================================================*/
/* External declarations.                                                    */
/*===========================================================================*/

#ifdef __cplusplus
extern "C" {
#endif
  void uartInit(void);
  void uartObjectInit(UARTDriver *uartp);
  void uartStart(UARTDriver *uartp, const UARTConfig *config);
  void uartStop(UARTDriver *uartp);
  void uartStartSend(UARTDriver *uartp, size_t n, const void *txbuf);
  void uartStartSendI(UARTDriver *uartp, size_t n, const void *txbuf);
  size_t uartStopSend(UARTDriver *uartp);
  size_t uartStopSendI(UARTDriver *uartp);
  void uartStartReceive(UARTDriver *uartp, size_t n, void *rxbuf);
  void uartStartReceiveI(UARTDriver *uartp, size_t n, void *rxbuf);
  size_t uartStopReceive(UARTDriver *uartp);
  size_t uartStopReceiveI(UARTDriver *uartp);
#ifdef __cplusplus
}
#endif

#endif /* HAL_USE_UART == TRUE */

#endif /* _UART_H_ */

/** @} */
