/*
 * comm_usb_serial.h
 *
 *  Created on: 8 okt 2015
 *      Author: benjamin
 */

#ifndef COMM_USB_SERIAL_H_
#define COMM_USB_SERIAL_H_

#ifdef __cplusplus
extern "C" {
#endif

// Variables
extern SerialUSBDriver SDU1;

// Functions
void comm_usb_serial_init(void);
int comm_usb_serial_is_active(void);

#ifdef __cplusplus
}
#endif

#endif /* COMM_USB_SERIAL_H_ */
