#ifndef COMM_USB_SERIAL_H_
#define COMM_USB_SERIAL_H_

// Variables
extern SerialUSBDriver SDU1;

// Functions
void comm_usb_serial_init(void);
int comm_usb_serial_is_active(void);

#endif /* COMM_USB_SERIAL_H_ */
