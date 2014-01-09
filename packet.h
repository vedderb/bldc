/*
 * packet.h
 *
 *  Created on: 21 mar 2013
 *      Author: benjamin
 */

#ifndef PACKET_H_
#define PACKET_H_

#include <stdint.h>

// Functions
void packet_init(void (*s_func)(unsigned char *data, unsigned char len),
		void (*p_func)(unsigned char *data, unsigned char len));
void packet_process_byte(uint8_t rx_data);
void packet_timerfunc(void);
void packet_send_packet(unsigned char *data, unsigned char len);

#endif /* PACKET_H_ */
