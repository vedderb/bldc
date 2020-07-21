/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#include "conf_general.h"
#include "utils.h"
#include <math.h>
#include HW_SOURCE

uint8_t hw_id_from_uuid(void) {
	return utils_crc32c(STM32_UUID_8, 12) & 0x7F;
}

#if defined(HW_ID_PIN_GPIOS) && defined(HW_ID_PIN_PINS)
uint8_t hw_id_from_pins(void) {
    stm32_gpio_t *hw_id_ports[]={HW_ID_PIN_GPIOS};
    const uint16_t hw_id_pins[] = {HW_ID_PIN_PINS};
    const uint16_t hw_id_pins_size = sizeof(hw_id_pins)/sizeof(uint16_t);

  const uint16_t DELAY_MS = 5;
  uint8_t trits[hw_id_pins_size];
  uint8_t id = 1u; //Start at 1
  for (uint8_t i=0; i<hw_id_pins_size; i++)
  {
    //Initialize pulldown
    palSetPadMode(hw_id_ports[i], hw_id_pins[i], PAL_MODE_INPUT_PULLDOWN);
    
    //Delay a little for the resistor to take affect
    chThdSleepMilliseconds(DELAY_MS);
    bool pin_set_pulldown = (palReadPad(hw_id_ports[i], hw_id_pins[i]));
    //Initialize pullup
    palSetPadMode(hw_id_ports[i], hw_id_pins[i], PAL_MODE_INPUT_PULLUP);
    //Delay a little for the resistor to take affect
    chThdSleepMilliseconds(DELAY_MS);
    bool pin_set_pullup = (palReadPad(hw_id_ports[i], hw_id_pins[i]));
    //Now determine the trit state
    if (!pin_set_pulldown && !pin_set_pullup)
    {
      //Tied to GND
      trits[i] = 1u;
      
    }
    else if (pin_set_pulldown && pin_set_pullup)
    {
      //Tied to VCC
      trits[i] = 2u;
    }
    else if (!pin_set_pulldown && pin_set_pullup)
    {
      //Floating
      trits[i] = 0u;
    }
    else
    {
      return hw_id_from_uuid();
      //To satisfy compiler warning
      trits[i] = 3u;
    }
    id += trits[i] * pow(3, i); 
    palSetPadMode(hw_id_ports[i], hw_id_pins[i], PAL_MODE_INPUT);
  }

    return id;
}
#endif //defined(HW_ID_PIN_GPIOS) && defined(HW_ID_PIN_PINS)
