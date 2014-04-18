#include "ch.h"
#include "hal.h"

#include "myUSB.h"
#include "usbdescriptor.h"
/*
 * Don't ask me, I have no idea what is done here...
 * I think most things are either self explanatory, well documented or not to be touched
 */

/*
 * USB Driver structure.
 */
SerialUSBDriver SDU1;


/*===========================================================================*/
/* USB related stuff. */
/*===========================================================================*/

/*
 * Endpoints to be used for USBD1.
 */
#define USBD1_DATA_REQUEST_EP 1
#define USBD1_DATA_AVAILABLE_EP 1
#define USBD1_INTERRUPT_REQUEST_EP 2

/*
 * Handles the GET_DESCRIPTOR callback. All required descriptors must be
 * handled here.
 */
static const USBDescriptor *get_descriptor(USBDriver *usbp,
		uint8_t dtype,
		uint8_t dindex,
		uint16_t lang) {

	(void)usbp;
	(void)lang;
	switch (dtype) {
	case USB_DESCRIPTOR_DEVICE:
		return &vcom_device_descriptor;
	case USB_DESCRIPTOR_CONFIGURATION:
		return &vcom_configuration_descriptor;
	case USB_DESCRIPTOR_STRING:
		if (dindex < 4)
			return &vcom_strings[dindex];
	}
	return NULL;
}

/**
 * @brief IN EP1 state.
 */
static USBInEndpointState ep1instate;

/**
 * @brief OUT EP1 state.
 */
static USBOutEndpointState ep1outstate;

/**
 * @brief EP1 initialization structure (both IN and OUT).
 */
static const USBEndpointConfig ep1config = {
		USB_EP_MODE_TYPE_BULK,
		NULL,
		sduDataTransmitted,
		sduDataReceived,
		0x0040,
		0x0040,
		&ep1instate,
		&ep1outstate,
		2,
		NULL
};

/**
 * @brief IN EP2 state.
 */
static USBInEndpointState ep2instate;

/**
 * @brief EP2 initialization structure (IN only).
 */
static const USBEndpointConfig ep2config = {
		USB_EP_MODE_TYPE_INTR,
		NULL,
		sduInterruptTransmitted,
		NULL,
		0x0010,
		0x0000,
		&ep2instate,
		NULL,
		1,
		NULL
};

/*
 * Handles the USB driver global events.
 */
static void usb_event(USBDriver *usbp, usbevent_t event) {
	extern SerialUSBDriver SDU1;

	switch (event) {
	case USB_EVENT_RESET:
		return;
	case USB_EVENT_ADDRESS:
		return;
	case USB_EVENT_CONFIGURED:
		chSysLockFromIsr();

		/* Enables the endpoints specified into the configuration.
Note, this callback is invoked from an ISR so I-Class functions
must be used.*/
		usbInitEndpointI(usbp, USBD1_DATA_REQUEST_EP, &ep1config);
		usbInitEndpointI(usbp, USBD1_INTERRUPT_REQUEST_EP, &ep2config);

		/* Resetting the state of the CDC subsystem.*/
		sduConfigureHookI(&SDU1);

		chSysUnlockFromIsr();
		return;
	case USB_EVENT_SUSPEND:
		return;
	case USB_EVENT_WAKEUP:
		return;
	case USB_EVENT_STALLED:
		return;
	}
	return;
}

/*
 * USB driver configuration.
 */
const USBConfig usbcfg = {
		usb_event,
		get_descriptor,
		sduRequestsHook,
		NULL
};

/*
 * Serial over USB driver configuration.
 */
const SerialUSBConfig serusbcfg = {
		&USBD1,
		USBD1_DATA_REQUEST_EP,
		USBD1_DATA_AVAILABLE_EP,
		USBD1_INTERRUPT_REQUEST_EP
};

int isUsbActive(void){
	return SDU1.config->usbp->state == USB_ACTIVE;
}

void myUSBinit(void){
	/*
	 * Initializes a serial-over-USB CDC driver.
	 */
	sduObjectInit(&SDU1);
	sduStart(&SDU1, &serusbcfg);

	/*
	 * Activates the USB driver and then the USB bus pull-up on D+.
	 * Note, a delay is inserted in order to not have to disconnect the cable
	 * after a reset.
	 */
	usbDisconnectBus(serusbcfg.usbp);
	chThdSleepMilliseconds(1000);
	usbStart(serusbcfg.usbp, &usbcfg);
	usbConnectBus(serusbcfg.usbp);
}
