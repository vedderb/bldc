/*
 * drv8301.h
 *
 *  Created on: 20 juli 2016
 *      Author: benjamin
 */

#ifndef HWCONF_DRV8301_H_
#define HWCONF_DRV8301_H_

// Functions
void drv8301_init(void);
void drv8301_set_oc_adj(int val);
unsigned int drv8301_read_reg(int reg);
void drv8301_write_reg(int reg, int data);

#endif /* HWCONF_DRV8301_H_ */
