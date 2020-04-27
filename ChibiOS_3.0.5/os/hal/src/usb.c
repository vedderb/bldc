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
 * @file    usb.c
 * @brief   USB Driver code.
 *
 * @addtogroup USB
 * @{
 */

#include <string.h>

#include "hal.h"

#if (HAL_USE_USB == TRUE) || defined(__DOXYGEN__)

/*===========================================================================*/
/* Driver local definitions.                                                 */
/*===========================================================================*/

/*===========================================================================*/
/* Driver exported variables.                                                */
/*===========================================================================*/

/*===========================================================================*/
/* Driver local variables and types.                                         */
/*===========================================================================*/

static const uint8_t zero_status[] = {0x00, 0x00};
static const uint8_t active_status[] ={0x00, 0x00};
static const uint8_t halted_status[] = {0x01, 0x00};

/*===========================================================================*/
/* Driver local functions.                                                   */
/*===========================================================================*/

static uint16_t get_hword(uint8_t *p) {
  uint16_t hw;

  hw  = (uint16_t)*p++;
  hw |= (uint16_t)*p << 8U;
  return hw;
}

/**
 * @brief  SET ADDRESS transaction callback.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 */
static void set_address(USBDriver *usbp) {

  usbp->address = usbp->setup[2];
  usb_lld_set_address(usbp);
  _usb_isr_invoke_event_cb(usbp, USB_EVENT_ADDRESS);
  usbp->state = USB_SELECTED;
}

/**
 * @brief   Standard requests handler.
 * @details This is the standard requests default handler, most standard
 *          requests are handled here, the user can override the standard
 *          handling using the @p requests_hook_cb hook in the
 *          @p USBConfig structure.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @return              The request handling exit code.
 * @retval false        Request not recognized by the handler or error.
 * @retval true         Request handled.
 */
static bool default_handler(USBDriver *usbp) {
  const USBDescriptor *dp;

  /* Decoding the request.*/
  switch ((((uint32_t)usbp->setup[0] & (USB_RTYPE_RECIPIENT_MASK |
                                        USB_RTYPE_TYPE_MASK)) |
           ((uint32_t)usbp->setup[1] << 8U))) {
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_GET_STATUS << 8):
    /* Just returns the current status word.*/
    usbSetupTransfer(usbp, (uint8_t *)&usbp->status, 2, NULL);
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_CLEAR_FEATURE << 8):
    /* Only the DEVICE_REMOTE_WAKEUP is handled here, any other feature
       number is handled as an error.*/
    if (usbp->setup[2] == USB_FEATURE_DEVICE_REMOTE_WAKEUP) {
      usbp->status &= ~2U;
      usbSetupTransfer(usbp, NULL, 0, NULL);
      return true;
    }
    return false;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_SET_FEATURE << 8):
    /* Only the DEVICE_REMOTE_WAKEUP is handled here, any other feature
       number is handled as an error.*/
    if (usbp->setup[2] == USB_FEATURE_DEVICE_REMOTE_WAKEUP) {
      usbp->status |= 2U;
      usbSetupTransfer(usbp, NULL, 0, NULL);
      return true;
    }
    return false;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_SET_ADDRESS << 8):
    /* The SET_ADDRESS handling can be performed here or postponed after
       the status packed depending on the USB_SET_ADDRESS_MODE low
       driver setting.*/
#if USB_SET_ADDRESS_MODE == USB_EARLY_SET_ADDRESS
    if ((usbp->setup[0] == USB_RTYPE_RECIPIENT_DEVICE) &&
        (usbp->setup[1] == USB_REQ_SET_ADDRESS)) {
      set_address(usbp);
    }
    usbSetupTransfer(usbp, NULL, 0, NULL);
#else
    usbSetupTransfer(usbp, NULL, 0, set_address);
#endif
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_GET_DESCRIPTOR << 8):
    /* Handling descriptor requests from the host.*/
    dp = usbp->config->get_descriptor_cb(usbp, usbp->setup[3],
                                         usbp->setup[2],
                                         get_hword(&usbp->setup[4]));
    if (dp == NULL) {
      return false;
    }
    /*lint -save -e9005 [11.8] Removing const is fine.*/
    usbSetupTransfer(usbp, (uint8_t *)dp->ud_string, dp->ud_size, NULL);
    /*lint -restore*/
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_GET_CONFIGURATION << 8):
    /* Returning the last selected configuration.*/
    usbSetupTransfer(usbp, &usbp->configuration, 1, NULL);
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_SET_CONFIGURATION << 8):
    /* Handling configuration selection from the host.*/
    usbp->configuration = usbp->setup[2];
    if (usbp->configuration == 0U) {
      usbp->state = USB_SELECTED;
    }
    else {
      usbp->state = USB_ACTIVE;
    }
    _usb_isr_invoke_event_cb(usbp, USB_EVENT_CONFIGURED);
    usbSetupTransfer(usbp, NULL, 0, NULL);
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_INTERFACE | ((uint32_t)USB_REQ_GET_STATUS << 8):
  case (uint32_t)USB_RTYPE_RECIPIENT_ENDPOINT | ((uint32_t)USB_REQ_SYNCH_FRAME << 8):
    /* Just sending two zero bytes, the application can change the behavior
       using a hook..*/
    /*lint -save -e9005 [11.8] Removing const is fine.*/
    usbSetupTransfer(usbp, (uint8_t *)zero_status, 2, NULL);
    /*lint -restore*/
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_ENDPOINT | ((uint32_t)USB_REQ_GET_STATUS << 8):
    /* Sending the EP status.*/
    if ((usbp->setup[4] & 0x80U) != 0U) {
      switch (usb_lld_get_status_in(usbp, usbp->setup[4] & 0x0FU)) {
      case EP_STATUS_STALLED:
        /*lint -save -e9005 [11.8] Removing const is fine.*/
        usbSetupTransfer(usbp, (uint8_t *)halted_status, 2, NULL);
        /*lint -restore*/
        return true;
      case EP_STATUS_ACTIVE:
        /*lint -save -e9005 [11.8] Removing const is fine.*/
        usbSetupTransfer(usbp, (uint8_t *)active_status, 2, NULL);
        /*lint -restore*/
        return true;
      case EP_STATUS_DISABLED:
      default:
        return false;
      }
    }
    else {
      switch (usb_lld_get_status_out(usbp, usbp->setup[4] & 0x0FU)) {
      case EP_STATUS_STALLED:
        /*lint -save -e9005 [11.8] Removing const is fine.*/
        usbSetupTransfer(usbp, (uint8_t *)halted_status, 2, NULL);
        /*lint -restore*/
        return true;
      case EP_STATUS_ACTIVE:
        /*lint -save -e9005 [11.8] Removing const is fine.*/
        usbSetupTransfer(usbp, (uint8_t *)active_status, 2, NULL);
        /*lint -restore*/
        return true;
      case EP_STATUS_DISABLED:
      default:
        return false;
      }
    }
  case (uint32_t)USB_RTYPE_RECIPIENT_ENDPOINT | ((uint32_t)USB_REQ_CLEAR_FEATURE << 8):
    /* Only ENDPOINT_HALT is handled as feature.*/
    if (usbp->setup[2] != USB_FEATURE_ENDPOINT_HALT) {
      return false;
    }
    /* Clearing the EP status, not valid for EP0, it is ignored in that case.*/
    if ((usbp->setup[4] & 0x0FU) != 0U) {
      if ((usbp->setup[4] & 0x80U) != 0U) {
        usb_lld_clear_in(usbp, usbp->setup[4] & 0x0FU);
      }
      else {
        usb_lld_clear_out(usbp, usbp->setup[4] & 0x0FU);
      }
    }
    usbSetupTransfer(usbp, NULL, 0, NULL);
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_ENDPOINT | ((uint32_t)USB_REQ_SET_FEATURE << 8):
    /* Only ENDPOINT_HALT is handled as feature.*/
    if (usbp->setup[2] != USB_FEATURE_ENDPOINT_HALT) {
      return false;
    }
    /* Stalling the EP, not valid for EP0, it is ignored in that case.*/
    if ((usbp->setup[4] & 0x0FU) != 0U) {
      if ((usbp->setup[4] & 0x80U) != 0U) {
        usb_lld_stall_in(usbp, usbp->setup[4] & 0x0FU);
      }
      else {
        usb_lld_stall_out(usbp, usbp->setup[4] & 0x0FU);
      }
    }
    usbSetupTransfer(usbp, NULL, 0, NULL);
    return true;
  case (uint32_t)USB_RTYPE_RECIPIENT_DEVICE | ((uint32_t)USB_REQ_SET_DESCRIPTOR << 8):
  case (uint32_t)USB_RTYPE_RECIPIENT_INTERFACE | ((uint32_t)USB_REQ_CLEAR_FEATURE << 8):
  case (uint32_t)USB_RTYPE_RECIPIENT_INTERFACE | ((uint32_t)USB_REQ_SET_FEATURE << 8):
  case (uint32_t)USB_RTYPE_RECIPIENT_INTERFACE | ((uint32_t)USB_REQ_GET_INTERFACE << 8):
  case (uint32_t)USB_RTYPE_RECIPIENT_INTERFACE | ((uint32_t)USB_REQ_SET_INTERFACE << 8):
    /* All the above requests are not handled here, if you need them then
       use the hook mechanism and provide handling.*/
  default:
    return false;
  }
}

/*===========================================================================*/
/* Driver exported functions.                                                */
/*===========================================================================*/

/**
 * @brief   USB Driver initialization.
 * @note    This function is implicitly invoked by @p halInit(), there is
 *          no need to explicitly initialize the driver.
 *
 * @init
 */
void usbInit(void) {

  usb_lld_init();
}

/**
 * @brief   Initializes the standard part of a @p USBDriver structure.
 *
 * @param[out] usbp     pointer to the @p USBDriver object
 *
 * @init
 */
void usbObjectInit(USBDriver *usbp) {
  unsigned i;

  usbp->state        = USB_STOP;
  usbp->config       = NULL;
  for (i = 0; i < (unsigned)USB_MAX_ENDPOINTS; i++) {
    usbp->in_params[i]  = NULL;
    usbp->out_params[i] = NULL;
  }
  usbp->transmitting = 0;
  usbp->receiving    = 0;
}

/**
 * @brief   Configures and activates the USB peripheral.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] config    pointer to the @p USBConfig object
 *
 * @api
 */
void usbStart(USBDriver *usbp, const USBConfig *config) {
  unsigned i;

  osalDbgCheck((usbp != NULL) && (config != NULL));

  osalSysLock();
  osalDbgAssert((usbp->state == USB_STOP) || (usbp->state == USB_READY),
                "invalid state");
  usbp->config = config;
  for (i = 0; i <= (unsigned)USB_MAX_ENDPOINTS; i++) {
    usbp->epc[i] = NULL;
  }
  usb_lld_start(usbp);
  usbp->state = USB_READY;
  osalSysUnlock();
}

/**
 * @brief   Deactivates the USB peripheral.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 *
 * @api
 */
void usbStop(USBDriver *usbp) {

  osalDbgCheck(usbp != NULL);

  osalSysLock();
  osalDbgAssert((usbp->state == USB_STOP) || (usbp->state == USB_READY) ||
                (usbp->state == USB_SELECTED) || (usbp->state == USB_ACTIVE) ||
                (usbp->state == USB_SUSPENDED),
                "invalid state");
  usb_lld_stop(usbp);
  usbp->state = USB_STOP;
  osalSysUnlock();
}

/**
 * @brief   Enables an endpoint.
 * @details This function enables an endpoint, both IN and/or OUT directions
 *          depending on the configuration structure.
 * @note    This function must be invoked in response of a SET_CONFIGURATION
 *          or SET_INTERFACE message.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 * @param[in] epcp      the endpoint configuration
 *
 * @iclass
 */
void usbInitEndpointI(USBDriver *usbp, usbep_t ep,
                      const USBEndpointConfig *epcp) {

  osalDbgCheckClassI();
  osalDbgCheck((usbp != NULL) && (epcp != NULL));
  osalDbgAssert(usbp->state == USB_ACTIVE,
                "invalid state");
  osalDbgAssert(usbp->epc[ep] == NULL, "already initialized");

  /* Logically enabling the endpoint in the USBDriver structure.*/
  if (epcp->in_state != NULL) {
    memset(epcp->in_state, 0, sizeof(USBInEndpointState));
  }
  if (epcp->out_state != NULL) {
    memset(epcp->out_state, 0, sizeof(USBOutEndpointState));
  }

  usbp->epc[ep] = epcp;

  /* Low level endpoint activation.*/
  usb_lld_init_endpoint(usbp, ep);
}

/**
 * @brief   Disables all the active endpoints.
 * @details This function disables all the active endpoints except the
 *          endpoint zero.
 * @note    This function must be invoked in response of a SET_CONFIGURATION
 *          message with configuration number zero.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 *
 * @iclass
 */
void usbDisableEndpointsI(USBDriver *usbp) {
  unsigned i;

  osalDbgCheckClassI();
  osalDbgCheck(usbp != NULL);
  osalDbgAssert(usbp->state == USB_SELECTED, "invalid state");

  usbp->transmitting &= ~1U;
  usbp->receiving    &= ~1U;
  for (i = 1; i <= (unsigned)USB_MAX_ENDPOINTS; i++) {
    usbp->epc[i] = NULL;
  }

  /* Low level endpoints deactivation.*/
  usb_lld_disable_endpoints(usbp);
}

/**
 * @brief   Prepares for a receive transaction on an OUT endpoint.
 * @post    The endpoint is ready for @p usbStartReceiveI().
 * @note    This function can be called both in ISR and thread context.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 * @param[out] buf      buffer where to copy the received data
 * @param[in] n         transaction size
 *
 * @special
 */
void usbPrepareReceive(USBDriver *usbp, usbep_t ep, uint8_t *buf, size_t n) {
  USBOutEndpointState *osp = usbp->epc[ep]->out_state;

  osp->rxqueued           = false;
  osp->mode.linear.rxbuf  = buf;
  osp->rxsize             = n;
  osp->rxcnt              = 0;

  usb_lld_prepare_receive(usbp, ep);
}

/**
 * @brief   Prepares for a transmit transaction on an IN endpoint.
 * @post    The endpoint is ready for @p usbStartTransmitI().
 * @note    This function can be called both in ISR and thread context.
 * @note    The queue must contain at least the amount of data specified
 *          as transaction size.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 * @param[in] buf       buffer where to fetch the data to be transmitted
 * @param[in] n         transaction size
 *
 * @special
 */
void usbPrepareTransmit(USBDriver *usbp, usbep_t ep,
                        const uint8_t *buf, size_t n) {
  USBInEndpointState *isp = usbp->epc[ep]->in_state;

  isp->txqueued           = false;
  isp->mode.linear.txbuf  = buf;
  isp->txsize             = n;
  isp->txcnt              = 0;

  usb_lld_prepare_transmit(usbp, ep);
}

/**
 * @brief   Prepares for a receive transaction on an OUT endpoint.
 * @post    The endpoint is ready for @p usbStartReceiveI().
 * @note    This function can be called both in ISR and thread context.
 * @note    The queue must have enough free space to accommodate the
 *          specified transaction size rounded to the next packet size
 *          boundary. For example if the transaction size is 1 and the
 *          packet size is 64 then the queue must have space for at least
 *          64 bytes.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 * @param[in] iqp       input queue to be filled with incoming data
 * @param[in] n         transaction size
 *
 * @special
 */
void usbPrepareQueuedReceive(USBDriver *usbp, usbep_t ep,
                             input_queue_t *iqp, size_t n) {
  USBOutEndpointState *osp = usbp->epc[ep]->out_state;

  osp->rxqueued           = true;
  osp->mode.queue.rxqueue = iqp;
  osp->rxsize             = n;
  osp->rxcnt              = 0;

  usb_lld_prepare_receive(usbp, ep);
}

/**
 * @brief   Prepares for a transmit transaction on an IN endpoint.
 * @post    The endpoint is ready for @p usbStartTransmitI().
 * @note    This function can be called both in ISR and thread context.
 * @note    The transmit transaction size is equal to the data contained
 *          in the queue.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 * @param[in] oqp       output queue to be fetched for outgoing data
 * @param[in] n         transaction size
 *
 * @special
 */
void usbPrepareQueuedTransmit(USBDriver *usbp, usbep_t ep,
                              output_queue_t *oqp, size_t n) {
  USBInEndpointState *isp = usbp->epc[ep]->in_state;

  isp->txqueued           = true;
  isp->mode.queue.txqueue = oqp;
  isp->txsize             = n;
  isp->txcnt              = 0;

  usb_lld_prepare_transmit(usbp, ep);
}

/**
 * @brief   Starts a receive transaction on an OUT endpoint.
 * @post    The endpoint callback is invoked when the transfer has been
 *          completed.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 *
 * @return              The operation status.
 * @retval false        Operation started successfully.
 * @retval true         Endpoint busy, operation not started.
 *
 * @iclass
 */
bool usbStartReceiveI(USBDriver *usbp, usbep_t ep) {

  osalDbgCheckClassI();
  osalDbgCheck(usbp != NULL);

  if (usbGetReceiveStatusI(usbp, ep)) {
    return true;
  }

  usbp->receiving |= (uint16_t)((unsigned)1U << (unsigned)ep);
  usb_lld_start_out(usbp, ep);
  return false;
}

/**
 * @brief   Starts a transmit transaction on an IN endpoint.
 * @post    The endpoint callback is invoked when the transfer has been
 *          completed.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 *
 * @return              The operation status.
 * @retval false        Operation started successfully.
 * @retval true         Endpoint busy, operation not started.
 *
 * @iclass
 */
bool usbStartTransmitI(USBDriver *usbp, usbep_t ep) {

  osalDbgCheckClassI();
  osalDbgCheck(usbp != NULL);

  if (usbGetTransmitStatusI(usbp, ep)) {
    return true;
  }

  usbp->transmitting |= (uint16_t)((unsigned)1U << (unsigned)ep);
  usb_lld_start_in(usbp, ep);
  return false;
}

/**
 * @brief   Stalls an OUT endpoint.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 *
 * @return              The operation status.
 * @retval false        Endpoint stalled.
 * @retval true         Endpoint busy, not stalled.
 *
 * @iclass
 */
bool usbStallReceiveI(USBDriver *usbp, usbep_t ep) {

  osalDbgCheckClassI();
  osalDbgCheck(usbp != NULL);

  if (usbGetReceiveStatusI(usbp, ep)) {
    return true;
  }

  usb_lld_stall_out(usbp, ep);
  return false;
}

/**
 * @brief   Stalls an IN endpoint.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number
 *
 * @return              The operation status.
 * @retval false        Endpoint stalled.
 * @retval true         Endpoint busy, not stalled.
 *
 * @iclass
 */
bool usbStallTransmitI(USBDriver *usbp, usbep_t ep) {

  osalDbgCheckClassI();
  osalDbgCheck(usbp != NULL);

  if (usbGetTransmitStatusI(usbp, ep)) {
    return true;
  }

  usb_lld_stall_in(usbp, ep);
  return false;
}

/**
 * @brief   USB reset routine.
 * @details This function must be invoked when an USB bus reset condition is
 *          detected.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 *
 * @notapi
 */
void _usb_reset(USBDriver *usbp) {
  unsigned i;

  /* State transition.*/
  usbp->state         = USB_READY;

  /* Resetting internal state.*/
  usbp->status        = 0;
  usbp->address       = 0;
  usbp->configuration = 0;
  usbp->transmitting  = 0;
  usbp->receiving     = 0;

  /* Invalidates all endpoints into the USBDriver structure.*/
  for (i = 0; i <= (unsigned)USB_MAX_ENDPOINTS; i++) {
    usbp->epc[i] = NULL;
  }

  /* EP0 state machine initialization.*/
  usbp->ep0state = USB_EP0_WAITING_SETUP;

  /* Low level reset.*/
  usb_lld_reset(usbp);

  /* Notification of reset event.*/
  _usb_isr_invoke_event_cb(usbp, USB_EVENT_RESET);
}

/**
 * @brief   USB suspend routine.
 * @details This function must be invoked when an USB bus suspend condition is
 *          detected.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 *
 * @notapi
 */
void _usb_suspend(USBDriver *usbp) {

  /* State transition.*/
  usbp->state         = USB_SUSPENDED;

  /* Notification of suspend event.*/
  _usb_isr_invoke_event_cb(usbp, USB_EVENT_SUSPEND);
}

/**
 * @brief   USB wake-up routine.
 * @details This function must be invoked when an USB bus wake-up condition is
 *          detected.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 *
 * @notapi
 */
void _usb_wakeup(USBDriver *usbp) {

  /* State transition.*/
  usbp->state         = USB_ACTIVE;

  /* Notification of suspend event.*/
  _usb_isr_invoke_event_cb(usbp, USB_EVENT_WAKEUP);
}

/**
 * @brief   Default EP0 SETUP callback.
 * @details This function is used by the low level driver as default handler
 *          for EP0 SETUP events.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number, always zero
 *
 * @notapi
 */
void _usb_ep0setup(USBDriver *usbp, usbep_t ep) {
  size_t max;

  usbp->ep0state = USB_EP0_WAITING_SETUP;
  usbReadSetup(usbp, ep, usbp->setup);

  /* First verify if the application has an handler installed for this
     request.*/
  /*lint -save -e9007 [13.5] No side effects, it is intentional.*/
  if ((usbp->config->requests_hook_cb == NULL) ||
      !(usbp->config->requests_hook_cb(usbp))) {
  /*lint -restore*/
    /* Invoking the default handler, if this fails then stalls the
       endpoint zero as error.*/
    /*lint -save -e9007 [13.5] No side effects, it is intentional.*/
    if (((usbp->setup[0] & USB_RTYPE_TYPE_MASK) != USB_RTYPE_TYPE_STD) ||
        !default_handler(usbp)) {
    /*lint -restore*/
      /* Error response, the state machine goes into an error state, the low
         level layer will have to reset it to USB_EP0_WAITING_SETUP after
         receiving a SETUP packet.*/
      usb_lld_stall_in(usbp, 0);
      usb_lld_stall_out(usbp, 0);
      _usb_isr_invoke_event_cb(usbp, USB_EVENT_STALLED);
      usbp->ep0state = USB_EP0_ERROR;
      return;
    }
  }
#if (USB_SET_ADDRESS_ACK_HANDLING == USB_SET_ADDRESS_ACK_HW)
  if (usbp->setup[1] == USB_REQ_SET_ADDRESS) {
    /* Zero-length packet sent by hardware */
    return;
  }
#endif
  /* Transfer preparation. The request handler must have populated
     correctly the fields ep0next, ep0n and ep0endcb using the macro
     usbSetupTransfer().*/
  max = (size_t)get_hword(&usbp->setup[6]);
  /* The transfer size cannot exceed the specified amount.*/
  if (usbp->ep0n > max) {
    usbp->ep0n = max;
  }
  if ((usbp->setup[0] & USB_RTYPE_DIR_MASK) == USB_RTYPE_DIR_DEV2HOST) {
    /* IN phase.*/
    if (usbp->ep0n != 0U) {
      /* Starts the transmit phase.*/
      usbp->ep0state = USB_EP0_TX;
      usbPrepareTransmit(usbp, 0, usbp->ep0next, usbp->ep0n);
      osalSysLockFromISR();
      (void) usbStartTransmitI(usbp, 0);
      osalSysUnlockFromISR();
    }
    else {
      /* No transmission phase, directly receiving the zero sized status
         packet.*/
      usbp->ep0state = USB_EP0_WAITING_STS;
#if (USB_EP0_STATUS_STAGE == USB_EP0_STATUS_STAGE_SW)
      usbPrepareReceive(usbp, 0, NULL, 0);
      osalSysLockFromISR();
      (void) usbStartReceiveI(usbp, 0);
      osalSysUnlockFromISR();
#else
      usb_lld_end_setup(usbp, ep);
#endif
    }
  }
  else {
    /* OUT phase.*/
    if (usbp->ep0n != 0U) {
      /* Starts the receive phase.*/
      usbp->ep0state = USB_EP0_RX;
      usbPrepareReceive(usbp, 0, usbp->ep0next, usbp->ep0n);
      osalSysLockFromISR();
      (void) usbStartReceiveI(usbp, 0);
      osalSysUnlockFromISR();
    }
    else {
      /* No receive phase, directly sending the zero sized status
         packet.*/
      usbp->ep0state = USB_EP0_SENDING_STS;
#if (USB_EP0_STATUS_STAGE == USB_EP0_STATUS_STAGE_SW)
      usbPrepareTransmit(usbp, 0, NULL, 0);
      osalSysLockFromISR();
      (void) usbStartTransmitI(usbp, 0);
      osalSysUnlockFromISR();
#else
      usb_lld_end_setup(usbp, ep);
#endif
    }
  }
}

/**
 * @brief   Default EP0 IN callback.
 * @details This function is used by the low level driver as default handler
 *          for EP0 IN events.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number, always zero
 *
 * @notapi
 */
void _usb_ep0in(USBDriver *usbp, usbep_t ep) {
  size_t max;

  (void)ep;
  switch (usbp->ep0state) {
  case USB_EP0_TX:
    max = (size_t)get_hword(&usbp->setup[6]);
    /* If the transmitted size is less than the requested size and it is a
       multiple of the maximum packet size then a zero size packet must be
       transmitted.*/
    if ((usbp->ep0n < max) &&
        ((usbp->ep0n % usbp->epc[0]->in_maxsize) == 0U)) {
      usbPrepareTransmit(usbp, 0, NULL, 0);
      osalSysLockFromISR();
      (void) usbStartTransmitI(usbp, 0);
      osalSysUnlockFromISR();
      usbp->ep0state = USB_EP0_WAITING_TX0;
      return;
    }
    /* Falls through. */
  case USB_EP0_WAITING_TX0:
    /* Transmit phase over, receiving the zero sized status packet.*/
    usbp->ep0state = USB_EP0_WAITING_STS;
#if (USB_EP0_STATUS_STAGE == USB_EP0_STATUS_STAGE_SW)
    usbPrepareReceive(usbp, 0, NULL, 0);
    osalSysLockFromISR();
    (void) usbStartReceiveI(usbp, 0);
    osalSysUnlockFromISR();
#else
    usb_lld_end_setup(usbp, ep);
#endif
    return;
  case USB_EP0_SENDING_STS:
    /* Status packet sent, invoking the callback if defined.*/
    if (usbp->ep0endcb != NULL) {
      usbp->ep0endcb(usbp);
    }
    usbp->ep0state = USB_EP0_WAITING_SETUP;
    return;
  case USB_EP0_WAITING_SETUP:
  case USB_EP0_WAITING_STS:
  case USB_EP0_RX:
    /* All the above are invalid states in the IN phase.*/
    osalDbgAssert(false, "EP0 state machine error");
    /* Falls through. */
  case USB_EP0_ERROR:
    /* Error response, the state machine goes into an error state, the low
       level layer will have to reset it to USB_EP0_WAITING_SETUP after
       receiving a SETUP packet.*/
    usb_lld_stall_in(usbp, 0);
    usb_lld_stall_out(usbp, 0);
    _usb_isr_invoke_event_cb(usbp, USB_EVENT_STALLED);
    usbp->ep0state = USB_EP0_ERROR;
    return;
  default:
    osalDbgAssert(false, "EP0 state machine invalid state");
  }
}

/**
 * @brief   Default EP0 OUT callback.
 * @details This function is used by the low level driver as default handler
 *          for EP0 OUT events.
 *
 * @param[in] usbp      pointer to the @p USBDriver object
 * @param[in] ep        endpoint number, always zero
 *
 * @notapi
 */
void _usb_ep0out(USBDriver *usbp, usbep_t ep) {

  (void)ep;
  switch (usbp->ep0state) {
  case USB_EP0_RX:
    /* Receive phase over, sending the zero sized status packet.*/
    usbp->ep0state = USB_EP0_SENDING_STS;
#if (USB_EP0_STATUS_STAGE == USB_EP0_STATUS_STAGE_SW)
    usbPrepareTransmit(usbp, 0, NULL, 0);
    osalSysLockFromISR();
    (void) usbStartTransmitI(usbp, 0);
    osalSysUnlockFromISR();
#else
    usb_lld_end_setup(usbp, ep);
#endif
    return;
  case USB_EP0_WAITING_STS:
    /* Status packet received, it must be zero sized, invoking the callback
       if defined.*/
#if (USB_EP0_STATUS_STAGE == USB_EP0_STATUS_STAGE_SW)
    if (usbGetReceiveTransactionSizeI(usbp, 0) != 0U) {
      break;
    }
#endif
    if (usbp->ep0endcb != NULL) {
      usbp->ep0endcb(usbp);
    }
    usbp->ep0state = USB_EP0_WAITING_SETUP;
    return;
  case USB_EP0_WAITING_SETUP:
  case USB_EP0_TX:
  case USB_EP0_WAITING_TX0:
  case USB_EP0_SENDING_STS:
    /* All the above are invalid states in the IN phase.*/
    osalDbgAssert(false, "EP0 state machine error");
    /* Falls through. */
  case USB_EP0_ERROR:
    /* Error response, the state machine goes into an error state, the low
       level layer will have to reset it to USB_EP0_WAITING_SETUP after
       receiving a SETUP packet.*/
    usb_lld_stall_in(usbp, 0);
    usb_lld_stall_out(usbp, 0);
    _usb_isr_invoke_event_cb(usbp, USB_EVENT_STALLED);
    usbp->ep0state = USB_EP0_ERROR;
    return;
  default:
    osalDbgAssert(false, "EP0 state machine invalid state");
  }
}

#endif /* HAL_USE_USB == TRUE */

/** @} */
