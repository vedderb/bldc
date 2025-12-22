/*
 * password.h
 *
 *  Created on: Nov 11, 2019
 *      Author: motorcontrol
 */

#ifndef PASSWORD_H_
#define PASSWORD_H_

#include "stdbool.h"
#include "stdint.h"

void password_init(void);
bool password_get_system_locked_flag(void);
void password_set_system_locked_flag( bool value);
bool password_get_system_enable_flag(void);
void password_set_system_enable_flag( bool value);
bool password_get_system_connection_alive(void);
void password_set_system_connection_alive(bool value);
void password_timeout_deinit(void);
void password_timeout_configure( uint32_t timeout_msec );
void password_timeout_reset(void);
void password_timeout_increment(uint16_t delta_msec);

#endif /* PASSWORD_H_ */
