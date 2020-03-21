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
 * @file    serial_usb.h
 * @brief   Serial over USB Driver macros and structures.
 *
 * @addtogroup SERIAL_USB
 * @{
 */

#ifndef _SERIAL_USB_H_
#define _SERIAL_USB_H_

#if (HAL_USE_SERIAL_USB == TRUE) || defined(__DOXYGEN__)

/*===========================================================================*/
/* Driver constants.                                                         */
/*===========================================================================*/

/**
 * @name    CDC specific messages.
 * @{
 */
#define CDC_SEND_ENCAPSULATED_COMMAND       0x00
#define CDC_GET_ENCAPSULATED_RESPONSE       0x01
#define CDC_SET_COMM_FEATURE                0x02
#define CDC_GET_COMM_FEATURE                0x03
#define CDC_CLEAR_COMM_FEATURE              0x04
#define CDC_SET_AUX_LINE_STATE              0x10
#define CDC_SET_HOOK_STATE                  0x11
#define CDC_PULSE_SETUP                     0x12
#define CDC_SEND_PULSE                      0x13
#define CDC_SET_PULSE_TIME                  0x14
#define CDC_RING_AUX_JACK                   0x15
#define CDC_SET_LINE_CODING                 0x20
#define CDC_GET_LINE_CODING                 0x21
#define CDC_SET_CONTROL_LINE_STATE          0x22
#define CDC_SEND_BREAK                      0x23
#define CDC_SET_RINGER_PARMS                0x30
#define CDC_GET_RINGER_PARMS                0x31
#define CDC_SET_OPERATION_PARMS             0x32
#define CDC_GET_OPERATION_PARMS             0x33
/** @} */

/**
 * @name    CDC classes
 * @{
 */
#define CDC_COMMUNICATION_INTERFACE_CLASS   0x02
#define CDC_DATA_INTERFACE_CLASS            0x0A
/** @} */

/**
 * @name    CDC subclasses
 * @{
 */
#define CDC_ABSTRACT_CONTROL_MODEL          0x02
/** @} */

/**
 * @name    CDC descriptors
 * @{
 */
#define CDC_CS_INTERFACE                    0x24
/** @} */

/**
 * @name    CDC subdescriptors
 * @{
 */
#define CDC_HEADER                          0x00
#define CDC_CALL_MANAGEMENT                 0x01
#define CDC_ABSTRACT_CONTROL_MANAGEMENT     0x02
#define CDC_UNION                           0x06
/** @} */

/**
 * @name    Line Control bit definitions.
 * @{
 */
#define LC_STOP_1                           0
#define LC_STOP_1P5                         1
#define LC_STOP_2                           2

#define LC_PARITY_NONE                      0
#define LC_PARITY_ODD                       1
#define LC_PARITY_EVEN                      2
#define LC_PARITY_MARK                      3
#define LC_PARITY_SPACE                     4
/** @} */

/*===========================================================================*/
/* Driver pre-compile time settings.                                         */
/*===========================================================================*/

/**
 * @name    SERIAL_USB configuration options
 * @{
 */
/**
 * @brief   Serial over USB buffers size.
 * @details Configuration parameter, the buffer size must be a multiple of
 *          the USB data endpoint maximum packet size.
 * @note    The default is 256 bytes for both the transmission and receive
 *          buffers.
 */
#if !defined(SERIAL_USB_BUFFERS_SIZE) || defined(__DOXYGEN__)
#define SERIAL_USB_BUFFERS_SIZE     256
#endif
/** @} */

/*===========================================================================*/
/* Derived constants and error checks.                                       */
/*===========================================================================*/

#if HAL_USE_USB == FALSE
#error "Serial over USB Driver requires HAL_USE_USB"
#endif

/*===========================================================================*/
/* Driver data structures and types.                                         */
/*===========================================================================*/

/**
 * @brief   Type of Line Coding structure.
 */
typedef struct {
  uint8_t                       dwDTERate[4];
  uint8_t                       bCharFormat;
  uint8_t                       bParityType;
  uint8_t                       bDataBits;
} cdc_linecoding_t;

/**
 * @brief Driver state machine possible states.
 */
typedef enum {
  SDU_UNINIT = 0,                   /**< Not initialized.                   */
  SDU_STOP = 1,                     /**< Stopped.                           */
  SDU_READY = 2                     /**< Ready.                             */
} sdustate_t;

/**
 * @brief   Structure representing a serial over USB driver.
 */
typedef struct SerialUSBDriver SerialUSBDriver;

/**
 * @brief   Serial over USB Driver configuration structure.
 * @details An instance of this structure must be passed to @p sduStart()
 *          in order to configure and start the driver operations.
 */
typedef struct {
  /**
   * @brief   USB driver to use.
   */
  USBDriver                 *usbp;
  /**
   * @brief   Bulk IN endpoint used for outgoing data transfer.
   */
  usbep_t                   bulk_in;
  /**
   * @brief   Bulk OUT endpoint used for incoming data transfer.
   */
  usbep_t                   bulk_out;
  /**
   * @brief   Interrupt IN endpoint used for notifications.
   * @note    If set to zero then the INT endpoint is assumed to be not
   *          present, USB descriptors must be changed accordingly.
   */
  usbep_t                   int_in;
} SerialUSBConfig;

/**
 * @brief   @p SerialDriver specific data.
 */
#define _serial_usb_driver_data                                             \
  _base_asynchronous_channel_data                                           \
  /* Driver state.*/                                                        \
  sdustate_t                state;                                          \
  /* Input queue.*/                                                         \
  input_queue_t             iqueue;                                         \
  /* Output queue.*/                                                        \
  output_queue_t            oqueue;                                         \
  /* Input buffer.*/                                                        \
  uint8_t                   ib[SERIAL_USB_BUFFERS_SIZE];                    \
  /* Output buffer.*/                                                       \
  uint8_t                   ob[SERIAL_USB_BUFFERS_SIZE];                    \
  /* End of the mandatory fields.*/                                         \
  /* Current configuration data.*/                                          \
  const SerialUSBConfig     *config;

/**
 * @brief   @p SerialUSBDriver specific methods.
 */
#define _serial_usb_driver_methods                                          \
  _base_asynchronous_channel_methods

/**
 * @extends BaseAsynchronousChannelVMT
 *
 * @brief   @p SerialDriver virtual methods table.
 */
struct SerialUSBDriverVMT {
  _serial_usb_driver_methods
};

/**
 * @extends BaseAsynchronousChannel
 *
 * @brief   Full duplex serial driver class.
 * @details This class extends @p BaseAsynchronousChannel by adding physical
 *          I/O queues.
 */
struct SerialUSBDriver {
  /** @brief Virtual Methods Table.*/
  const struct SerialUSBDriverVMT *vmt;
  _serial_usb_driver_data
};

/*===========================================================================*/
/* Driver macros.                                                            */
/*===========================================================================*/

/*===========================================================================*/
/* External declarations.                                                    */
/*===========================================================================*/

#ifdef __cplusplus
extern "C" {
#endif
  void sduInit(void);
  void sduObjectInit(SerialUSBDriver *sdup);
  void sduStart(SerialUSBDriver *sdup, const SerialUSBConfig *config);
  void sduStop(SerialUSBDriver *sdup);
  void sduConfigureHookI(SerialUSBDriver *sdup);
  bool sduRequestsHook(USBDriver *usbp);
  void sduDataTransmitted(USBDriver *usbp, usbep_t ep);
  void sduDataReceived(USBDriver *usbp, usbep_t ep);
  void sduInterruptTransmitted(USBDriver *usbp, usbep_t ep);
#ifdef __cplusplus
}
#endif

#endif /* HAL_USE_SERIAL_USB == TRUE */

#endif /* _SERIAL_USB_H_ */

/** @} */
