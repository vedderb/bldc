#ifndef USBDESCRIPTOR_H_INCLUDED
#define USBDESCRIPTOR_H_INCLUDED

/*
* Endpoints to be used for USBD1.
*/
#define USBD1_DATA_REQUEST_EP 1
#define USBD1_DATA_AVAILABLE_EP 1
#define USBD1_INTERRUPT_REQUEST_EP 2

/*
* USB Device Descriptor.
*/
static const uint8_t vcom_device_descriptor_data[18] = {
  USB_DESC_DEVICE (0x0110, /* bcdUSB (1.1). */
                         0x02, /* bDeviceClass (CDC). */
                         0x00, /* bDeviceSubClass. */
                         0x00, /* bDeviceProtocol. */
                         0x40, /* bMaxPacketSize. */
                         0x0483, /* idVendor (ST). */
                         0x5740, /* idProduct. */
                         0x0200, /* bcdDevice. */
                         1, /* iManufacturer. */
                         2, /* iProduct. */
                         3, /* iSerialNumber. */
                         1) /* bNumConfigurations. */
};

/*
* Device Descriptor wrapper.
*/
static const USBDescriptor vcom_device_descriptor = {
  sizeof vcom_device_descriptor_data,
  vcom_device_descriptor_data
};

/* Configuration Descriptor tree for a CDC.*/
static const uint8_t vcom_configuration_descriptor_data[67] = {
  /* Configuration Descriptor.*/
  USB_DESC_CONFIGURATION(67, /* wTotalLength. */
                         0x02, /* bNumInterfaces. */
                         0x01, /* bConfigurationValue. */
                         0, /* iConfiguration. */
                         0xC0, /* bmAttributes (self powered). */
                         50), /* bMaxPower (100mA). */
  /* Interface Descriptor.*/
  USB_DESC_INTERFACE (0x00, /* bInterfaceNumber. */
                         0x00, /* bAlternateSetting. */
                         0x01, /* bNumEndpoints. */
                         0x02, /* bInterfaceClass (Communications
Interface Class, CDC section
4.2). */
                         0x02, /* bInterfaceSubClass (Abstract
Control Model, CDC section 4.3). */
                         0x01, /* bInterfaceProtocol (AT commands,
CDC section 4.4). */
                         0), /* iInterface. */
  /* Header Functional Descriptor (CDC section 5.2.3).*/
  USB_DESC_BYTE (5), /* bLength. */
  USB_DESC_BYTE (0x24), /* bDescriptorType (CS_INTERFACE). */
  USB_DESC_BYTE (0x00), /* bDescriptorSubtype (Header
Functional Descriptor. */
  USB_DESC_BCD (0x0110), /* bcdCDC. */
  /* Call Management Functional Descriptor. */
  USB_DESC_BYTE (5), /* bFunctionLength. */
  USB_DESC_BYTE (0x24), /* bDescriptorType (CS_INTERFACE). */
  USB_DESC_BYTE (0x01), /* bDescriptorSubtype (Call Management
Functional Descriptor). */
  USB_DESC_BYTE (0x00), /* bmCapabilities (D0+D1). */
  USB_DESC_BYTE (0x01), /* bDataInterface. */
  /* ACM Functional Descriptor.*/
  USB_DESC_BYTE (4), /* bFunctionLength. */
  USB_DESC_BYTE (0x24), /* bDescriptorType (CS_INTERFACE). */
  USB_DESC_BYTE (0x02), /* bDescriptorSubtype (Abstract
Control Management Descriptor). */
  USB_DESC_BYTE (0x02), /* bmCapabilities. */
  /* Union Functional Descriptor.*/
  USB_DESC_BYTE (5), /* bFunctionLength. */
  USB_DESC_BYTE (0x24), /* bDescriptorType (CS_INTERFACE). */
  USB_DESC_BYTE (0x06), /* bDescriptorSubtype (Union
Functional Descriptor). */
  USB_DESC_BYTE (0x00), /* bMasterInterface (Communication
Class Interface). */
  USB_DESC_BYTE (0x01), /* bSlaveInterface0 (Data Class
Interface). */
  /* Endpoint 2 Descriptor.*/
  USB_DESC_ENDPOINT (USBD1_INTERRUPT_REQUEST_EP|0x80,
                         0x03, /* bmAttributes (Interrupt). */
                         0x0008, /* wMaxPacketSize. */
                         0xFF), /* bInterval. */
  /* Interface Descriptor.*/
  USB_DESC_INTERFACE (0x01, /* bInterfaceNumber. */
                         0x00, /* bAlternateSetting. */
                         0x02, /* bNumEndpoints. */
                         0x0A, /* bInterfaceClass (Data Class
Interface, CDC section 4.5). */
                         0x00, /* bInterfaceSubClass (CDC section
4.6). */
                         0x00, /* bInterfaceProtocol (CDC section
4.7). */
                         0x00), /* iInterface. */
  /* Endpoint 3 Descriptor.*/
  USB_DESC_ENDPOINT (USBD1_DATA_AVAILABLE_EP, /* bEndpointAddress.*/
                         0x02, /* bmAttributes (Bulk). */
                         0x0040, /* wMaxPacketSize. */
                         0x00), /* bInterval. */
  /* Endpoint 1 Descriptor.*/
  USB_DESC_ENDPOINT (USBD1_DATA_REQUEST_EP|0x80, /* bEndpointAddress.*/
                         0x02, /* bmAttributes (Bulk). */
                         0x0040, /* wMaxPacketSize. */
                         0x00) /* bInterval. */
};

/*
* Configuration Descriptor wrapper.
*/
static const USBDescriptor vcom_configuration_descriptor = {
  sizeof vcom_configuration_descriptor_data,
  vcom_configuration_descriptor_data
};

/*
* U.S. English language identifier.
*/
static const uint8_t vcom_string0[] = {
  USB_DESC_BYTE(4), /* bLength. */
  USB_DESC_BYTE(USB_DESCRIPTOR_STRING), /* bDescriptorType. */
  USB_DESC_WORD(0x0409) /* wLANGID (U.S. English). */
};

/*
* Vendor string.
*/
static const uint8_t vcom_string1[] = {
  USB_DESC_BYTE(38), /* bLength. */
  USB_DESC_BYTE(USB_DESCRIPTOR_STRING), /* bDescriptorType. */
  'S', 0, 'T', 0, 'M', 0, 'i', 0, 'c', 0, 'r', 0, 'o', 0, 'e', 0,
  'l', 0, 'e', 0, 'c', 0, 't', 0, 'r', 0, 'o', 0, 'n', 0, 'i', 0,
  'c', 0, 's', 0
};

/*
* Device Description string.
*/
static const uint8_t vcom_string2[] = {
  USB_DESC_BYTE(56), /* bLength. */
  USB_DESC_BYTE(USB_DESCRIPTOR_STRING), /* bDescriptorType. */
  'C', 0, 'h', 0, 'i', 0, 'b', 0, 'i', 0, 'O', 0, 'S', 0, '/', 0,
  'R', 0, 'T', 0, ' ', 0, 'V', 0, 'i', 0, 'r', 0, 't', 0, 'u', 0,
  'a', 0, 'l', 0, ' ', 0, 'C', 0, 'O', 0, 'M', 0, ' ', 0, 'P', 0,
  'o', 0, 'r', 0, 't', 0
};

/*
* Serial Number string.
*/
static const uint8_t vcom_string3[] = {
  USB_DESC_BYTE(8), /* bLength. */
  USB_DESC_BYTE(USB_DESCRIPTOR_STRING), /* bDescriptorType. */
  '0' + CH_KERNEL_MAJOR, 0,
  '0' + CH_KERNEL_MINOR, 0,
  '0' + CH_KERNEL_PATCH, 0
};

/*
* Strings wrappers array.
*/
static const USBDescriptor vcom_strings[] = {
  {sizeof vcom_string0, vcom_string0},
  {sizeof vcom_string1, vcom_string1},
  {sizeof vcom_string2, vcom_string2},
  {sizeof vcom_string3, vcom_string3}
};



#endif // USBDESCRIPTOR_H_INCLUDED
