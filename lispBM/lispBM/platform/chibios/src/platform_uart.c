/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se
                   Benjamin Vedder

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

#include "platform_uart.h"
#include "lbm_types.h"
#include "symrepr.h"
#include "heap.h"
#include "extensions.h"
#include <string.h>

#include "platform_chibios_conf.h"

#if !defined(LBM_UART_0) && !defined(LBM_UART_1) && !defined(LBM_UART_2) && !defined(LBM_UART_3)
#warning No UART configured
#endif

typedef struct {
  SerialConfig *cfg;
  SerialDriver *drv;
  ioportid_t   tx_gpio;
  ioportid_t   rx_gpio;
  int          tx_pin;
  int          rx_pin;
  int          tx_pin_mode;
  int          rx_pin_mode;
} lbm_uart_if_t;


// 4 uarts made available
#ifdef LBM_UART_0
static SerialConfig uart_cfg0 = {
  115200, 0, 0, 0
};
static lbm_uart_if_t lbm_uart_0 =
  {
   &uart_cfg0,
   &LBM_UART_0,
   LBM_UART_0_TX_GPIO,
   LBM_UART_0_RX_GPIO,
   LBM_UART_0_TX_PIN,
   LBM_UART_0_RX_PIN,
   LBM_UART_0_TX_PIN_MODE,
   LBM_UART_0_TX_PIN_MODE
  };
static lbm_value uart_0_sym = 0;
#endif
#ifdef LBM_UART_1
static SerialConfig uart_cfg1 = {
  115200, 0, 0, 0
};
static lbm_uart_if_t lbm_uart_1 =
  {
   &uart_cfg1,
   &LBM_UART_1,
   LBM_UART_1_TX_GPIO,
   LBM_UART_1_RX_GPIO,
   LBM_UART_1_TX_PIN,
   LBM_UART_1_RX_PIN,
   LBM_UART_1_TX_PIN_MODE,
   LBM_UART_1_TX_PIN_MODE
  };
static lbm_value uart_1_sym = 0;
#endif
#ifdef LBM_UART_2
static SerialConfig uart_cfg2 = {
  115200, 0, 0, 0
};
static lbm_uart_if_t lbm_uart_2 =
  {
   &uart_cfg2,
   &LBM_UART_2,
   LBM_UART_2_TX_GPIO,
   LBM_UART_2_RX_GPIO,
   LBM_UART_2_TX_PIN,
   LBM_UART_2_RX_PIN,
   LBM_UART_2_TX_PIN_MODE,
   LBM_UART_2_TX_PIN_MODE
  };
static lbm_value uart_2_sym = 0;
#endif
#ifdef LBM_UART_3
static SerialConfig uart_cfg3 = {
  115200, 0, 0, 0
};
static lbm_uart_if_t lbm_uart_3 =
  {
   &uart_cfg3,
   &LBM_UART_3,
   LBM_UART_3_TX_GPIO,
   LBM_UART_3_RX_GPIO,
   LBM_UART_3_TX_PIN,
   LBM_UART_3_RX_PIN,
   LBM_UART_3_TX_PIN_MODE,
   LBM_UART_3_TX_PIN_MODE
  };
static lbm_value uart_3_sym = 0;
#endif

static lbm_uart_if_t *get_uart_if(lbm_value uart) {
  #ifdef LBM_UART_0
  if (uart == uart_0_sym)
    return &lbm_uart_0;
  #endif
  #ifdef LBM_UART_1
  if (uart == uart_1_sym)
    return &lbm_uart_1;
  #endif
  #ifdef LBM_UART_2
  if (uart == uart_1_sym)
    return &lbm_uart_0;
  #endif
  #ifdef LBM_UART_3
  if (uart == uart_1_sym)
    return &lbm_uart_1;
  #endif
  return NULL;
}

lbm_value ext_uart_init(lbm_value *args, lbm_uint argn){

  if (argn != 2)
    return ENC_SYM_NIL;

  if (!lbm_is_symbol(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;

  lbm_uint baud = lbm_dec_as_u32(args[1]);
  lbm_uint uart = lbm_dec_as_u32(args[0]);

  lbm_uart_if_t  *lbmu = get_uart_if(uart);
  if (lbmu) {
    SerialConfig *cfg = lbmu->cfg;
    SerialDriver *drv = lbmu->drv;

    memset(cfg, 0 , sizeof(SerialConfig));
    cfg->speed = baud;
    palSetPadMode(lbmu->tx_gpio,
                  lbmu->tx_pin,
                  lbmu->tx_pin_mode);
    palSetPadMode(lbmu->rx_gpio,
                  lbmu->rx_pin,
                  lbmu->rx_pin_mode);
    sdStart(drv, cfg);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}


static lbm_value ext_uart_write(lbm_value *args, lbm_uint argn) {

  if (argn != 2 || (!lbm_is_symbol(args[0]) ||
                    (lbm_type_of(args[1]) != LBM_TYPE_CONS &&
                     lbm_type_of(args[1]) != LBM_TYPE_ARRAY))) {
    return ENC_SYM_TERROR;
  }

  const int max_len = 20;
  uint8_t to_send[max_len];
  uint8_t *to_send_ptr = to_send;
  int ind = 0;

  int uart = lbm_dec_as_i32(args[0]);

  if (lbm_type_of(args[1]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
    if (array->elt_type != LBM_TYPE_BYTE) {
      return lbm_enc_sym(SYM_EERROR);
    }

    to_send_ptr = (uint8_t*)array->data;
    ind = array->size;
  } else {
    lbm_value curr = args[1];
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      lbm_value  arg = lbm_car(curr);

      if (lbm_is_number(arg)) {
        to_send[ind++] = lbm_dec_as_u32(arg);
      } else {
        return lbm_enc_sym(SYM_EERROR);
      }

      if (ind == max_len) {
        break;
      }

      curr = lbm_cdr(curr);
    }
  }

  lbm_uart_if_t *lbmu = get_uart_if(uart);
  if (lbmu) {
    SerialDriver *drv = lbmu->drv;
    sdWrite(drv, to_send_ptr, ind);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}

static lbm_value ext_uart_read(lbm_value *args, lbm_uint argn) {
  if ((argn != 3 && argn != 4 && argn != 5) ||
      !lbm_is_symbol(args[0]) ||
      lbm_type_of(args[1]) != LBM_TYPE_ARRAY || !lbm_is_number(args[2])) {
    return lbm_enc_sym(SYM_TERROR);
  }

  int uart = lbm_dec_as_i32(args[0]);

  lbm_uart_if_t *lbmu = get_uart_if(uart);
  if (!lbmu) {
    return lbm_enc_sym(SYM_FATAL_ERROR);
  }
  SerialDriver *drv = lbmu->drv;

  unsigned int num = lbm_dec_as_u32(args[2]);
  if (num > 512) {
    return lbm_enc_sym(SYM_TERROR);
  }

  if (num == 0) {
    return lbm_enc_i(0);
  }

  unsigned int offset = 0;
  if (argn >= 4) {
    if (!lbm_is_number(args[3])) {
      return lbm_enc_sym(SYM_TERROR);
    }
    offset = lbm_dec_as_u32(args[3]);
  }

  int stop_at = -1;
  if (argn >= 5) {
    if (!lbm_is_number(args[4])) {
      return lbm_enc_sym(SYM_TERROR);
    }
    stop_at = lbm_dec_as_u32(args[4]);
  }

  lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
  if (array->elt_type != LBM_TYPE_BYTE || array->size < (num + offset)) {
    return lbm_enc_sym(SYM_EERROR);
  }

  unsigned int count = 0;
  msg_t res = sdGetTimeout(drv, TIME_IMMEDIATE);
  while (res != MSG_TIMEOUT) {
    ((uint8_t*)array->data)[offset + count] = (uint8_t)res;
    count++;
    if (res == stop_at || count >= num) {
      break;
    }
    res = sdGetTimeout(drv, TIME_IMMEDIATE);
  }

  return lbm_enc_i(count);
}


static lbm_value ext_uart_in_avail(lbm_value *args, lbm_uint argn) {

  if (argn != 1 || (!lbm_is_symbol(args[0]))) {
    return ENC_SYM_TERROR;
  }

  int uart = lbm_dec_as_i32(args[0]);
  lbm_uart_if_t *lbmu = get_uart_if(uart);
  if (!lbmu)
    return ENC_SYM_FATAL_ERROR;

  SerialDriver *drv = lbmu->drv;

  int avail = qSpaceI(&drv->iqueue);
  return lbm_enc_i(avail);
}

static lbm_value ext_uart_out_avail(lbm_value *args, lbm_uint argn) {

  if (argn != 1 || (!lbm_is_symbol(args[0]))) {
    return ENC_SYM_TERROR;
  }

  int uart = lbm_dec_as_i32(args[0]);

  lbm_uart_if_t *lbmu = get_uart_if(uart);
  if (!lbmu)
    return ENC_SYM_FATAL_ERROR;

  SerialDriver *drv = lbmu->drv;

  int avail = qSpaceI(&drv->oqueue);
  return lbm_enc_i(avail);
}

// Interface

bool platform_uart_init(void) {
  int res = 1;

  res = res && lbm_add_extension("uart-init", ext_uart_init);
  res = res && lbm_add_extension("uart-write", ext_uart_write);
  res = res && lbm_add_extension("uart-read", ext_uart_read);
  res = res && lbm_add_extension("uart-in-avail", ext_uart_in_avail);
  res = res && lbm_add_extension("uart-out-avail", ext_uart_out_avail);

#ifdef LBM_UART_0
  res = res && lbm_str_to_symbol("uart-0", &uart_0_sym);
#endif
#ifdef LBM_UART_1
  res = res && lbm_str_to_symbol("uart-1", &uart_1_sym);
#endif
#ifdef LBM_UART_2
  res = res && lbm_str_to_symbol("uart-2", &uart_2_sym);
#endif
#ifdef LBM_UART_3
  res = res && lbm_str_to_symbol("uart-3", &uart_3_sym);
#endif
  return res;
}
