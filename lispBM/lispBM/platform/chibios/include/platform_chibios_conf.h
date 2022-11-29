/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef PLATFORM_CHIBIOS_CONF_H_
#define PLATFORM_CHBIOS_CONF_H_

#warning "Default chibios configuration used"

// LBM_UART_0 - LBM_UART_3
#define LBM_UART_0 SD4
#define LBM_UART_0_TX_GPIO GPIOA
#define LBM_UART_0_RX_GPIO GPIOA
#define LBM_UART_0_TX_PIN 0
#define LBM_UART_0_RX_PIN 1
#define LBM_UART_0_TX_PIN_MODE PAL_MODE_ALTERNATE(8)
#define LBM_UART_0_RX_PIN_MODE PAL_MODE_ALTERNATE(8)

#endif
