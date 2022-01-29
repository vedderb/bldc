
/* Hello World Example

   This example code is in the Public Domain (or CC0 licensed, at your option.)

   Unless required by applicable law or agreed to in writing, this
   software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, either express or implied.
*/
#include <stdio.h>
#include <ctype.h>

#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_system.h"
#include "esp_spi_flash.h"
#include "esp_rom_gpio.h"
#include "driver/uart.h"
#include "soc/uart_periph.h"
#include "led_strip.h"

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "extensions.h"
#include "env.h"
#include "memory.h"

#define DEFAULT_UART_CHANNEL    UART_NUM_1
#define UARTS_BAUD_RATE         (115200)
#define DEFAULT_UART_RX_PIN     (2)
#define DEFAULT_UART_TX_PIN     (3)
#define READ_BUF_SIZE           (1024)

#define EVAL_CPS_STACK_SIZE 256

#define CONFIG_BLINK_LED_RMT_CHANNEL 0
#define BLINK_GPIO 8

// LED STUFF

static led_strip_t *pStrip_a;

static void configure_led(void)
{
   
    pStrip_a = led_strip_init(CONFIG_BLINK_LED_RMT_CHANNEL, BLINK_GPIO, 1);
    pStrip_a->clear(pStrip_a, 50);
}

// UART 
static void configure_uart(void)
{
    /* Configure parameters of an UART driver,
     * communication pins and install the driver */
    uart_config_t uart_config = {
        .baud_rate = UARTS_BAUD_RATE,
        .data_bits = UART_DATA_8_BITS,
        .parity    = UART_PARITY_DISABLE,
        .stop_bits = UART_STOP_BITS_1,
        .flow_ctrl = UART_HW_FLOWCTRL_DISABLE,
        .source_clk = UART_SCLK_APB,
    };

    ESP_ERROR_CHECK(uart_driver_install(DEFAULT_UART_CHANNEL, READ_BUF_SIZE * 2, 0, 0, NULL, 0));
    ESP_ERROR_CHECK(uart_param_config(DEFAULT_UART_CHANNEL, &uart_config));
    ESP_ERROR_CHECK(uart_set_pin(DEFAULT_UART_CHANNEL, DEFAULT_UART_TX_PIN, DEFAULT_UART_RX_PIN,
                                 UART_PIN_NO_CHANGE, UART_PIN_NO_CHANGE));


}

int get_char(void) {
  int len = 0;
  int c;
  do {
    len = uart_read_bytes(DEFAULT_UART_CHANNEL, &c, 1, 100 / portTICK_RATE_MS);
  } while (len == 0);
  return c;
}

void put_char(char c) {
   uart_write_bytes(DEFAULT_UART_CHANNEL, &c, 1);
}


void uart_print(char *str) {

  char c = *str;;

  while (c != 0) {
    
    put_char(c);
    c = *(++str); 
  }
}

int inputline(char *buffer, int size) {
  int n = 0;
  unsigned char c;
  for (n = 0; n < size - 1; n++) {

    c = get_char();
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      put_char(0x8); /* output backspace character */
      put_char(' ');
      put_char(0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        put_char(c);
        buffer[n] = c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

lbm_value ext_led(lbm_value *args, lbm_uint argn) {

  if (argn != 1) return lbm_enc_sym(symrepr_nil);

  unsigned char r,g, b;

  lbm_value v = args[0]; 

  if (! lbm_is_ptr(v)) return lbm_enc_sym(symrepr_nil);

  r = (unsigned char)lbm_dec_i(lbm_car(v));
  g = (unsigned char)lbm_dec_i(lbm_car(lbm_cdr(v)));
  b = (unsigned char)lbm_dec_i(lbm_car(lbm_cdr(lbm_cdr(v))));

  pStrip_a->set_pixel(pStrip_a, 0, r, g, b);
  /* Refresh the strip to send data */
  pStrip_a->refresh(pStrip_a, 100);

  return lbm_enc_sym(symrepr_true);
  
  
}


void app_main(void)
{
  printf("Hello world!\n");

  /* Print chip information */
  esp_chip_info_t chip_info;
  esp_chip_info(&chip_info);
  printf("This is %s chip with %d CPU core(s), WiFi%s%s, ",
	 CONFIG_IDF_TARGET,
	 chip_info.cores,
	 (chip_info.features & CHIP_FEATURE_BT) ? "/BT" : "",
	 (chip_info.features & CHIP_FEATURE_BLE) ? "/BLE" : "");

  printf("silicon revision %d, ", chip_info.revision);

  printf("%dMB %s flash\n", spi_flash_get_chip_size() / (1024 * 1024),
	 (chip_info.features & CHIP_FEATURE_EMB_FLASH) ? "embedded" : "external");

  printf("Minimum free heap size: %d bytes\n", esp_get_minimum_free_heap_size());

  fflush(stdout);
  configure_uart();
  printf("Uart configured\n");

  uart_flush(DEFAULT_UART_CHANNEL);
  
  // Initialize LispBM
    
  int res = 0;
  char out_str[1024];
  char out_err[1024];
  char str[1024];
  
  unsigned char *memory = malloc(MEMORY_SIZE_16K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_16K);

  res = lbm_memory_init(memory, MEMORY_SIZE_16K,
  		    bitmap, MEMORY_BITMAP_SIZE_16K);

  snprintf(out_str,1024,"Memory initialized. Memory size: %u Words. Free: %u Words.\n",
  	   lbm_memory_num_words(), lbm_memory_num_free());
  
  if (res)
    uart_print(out_str);
  else {
    printf("Error initializing memory!\n");
    vTaskDelay(3000 / portTICK_PERIOD_MS);
    esp_restart();
  }

  res = lbm_symrepr_init();
  if (res)
    uart_print("Symrepr initialized.\r\n");
  else {
    uart_print("Error initializing symrepr!\r\n");
    vTaskDelay(3000 / portTICK_PERIOD_MS);
    esp_restart();
  }

  res = lbm_heap_init(4096);
  if (res)
    uart_print("Heap initialized.\r\n");
  else {
    uart_print("Error initializing heap.\r\n");

    vTaskDelay(3000 / portTICK_PERIOD_MS);
    esp_restart();
  }

  res = eval_cps_init_nc(EVAL_CPS_STACK_SIZE, false);
  if (res)
    uart_print("Evaluator initialized.\r\n");
  else {
    uart_print("Error initializing evaluator.\r\n");
    vTaskDelay(3000 / portTICK_PERIOD_MS);
    esp_restart();
  }

  res = lbm_add_extension("led", ext_led);
  if (res)
    uart_print("Extension added.\n");
  else
    uart_print("Error adding extension.\n");


  lbm_value prelude = prelude_load();
  eval_cps_program_nc(prelude);

  snprintf(out_str,1024,"Lisp REPL started on %s chip with %d CPU core(s), WiFi%s%s, \n",
  	   CONFIG_IDF_TARGET,
  	   chip_info.cores,
  	   (chip_info.features & CHIP_FEATURE_BT) ? "/BT" : "",
  	   (chip_info.features & CHIP_FEATURE_BLE) ? "/BLE" : "");

  
  uart_print(out_str);
  uart_print("Type :quit to exit.\n");
  uart_print("     :info for statistics.\n");


  configure_led();
  
  while (1) {
    uart_print("# ");

    memset(str, 0, 1024);
    
    int n = inputline(str, 1024);
    uart_print("\n");

    
    if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      break;
    } else if ( n == 1 && (strncmp(str, "\n", 1) == 0 ||
  			   strncmp(str, "\r", 1) == 0 )) {

      uart_print("\n# ");

    } else if (n == 0)  {
      continue;
    } else {
      lbm_value t;
      t = tokpar_parse(str);
      
      lbm_value r_r = eval_cps_program_nc(t);
      
      int r = lbm_print_value(out_str, 1024, out_err, 1024, r_r);
      if (r >= 0) {
  	uart_print("> ");
  	uart_print(out_str);
  	uart_print("\n");
      } else {
  	uart_print("(error)> ");
  	uart_print(out_err);
  	uart_print("\n");
      }
    }
    
  }

  /* unsigned char r,g,b; */

  /* r = 17; g = 200; b = 97;  */
  
  /* while (1) { */
  /*    pStrip_a->set_pixel(pStrip_a, 0, r, g, b); */
  /*       /\* Refresh the strip to send data *\/ */
  /*    pStrip_a->refresh(pStrip_a, 100); */

  /*    r ++; */
  /*    g ++; */
  /*    b ++; */
  /*    vTaskDelay(10 / portTICK_PERIOD_MS); */
  /* } */

  
  vTaskDelay(3000 / portTICK_PERIOD_MS);
  printf("Restarting now.\n");
  fflush(stdout);
  esp_restart();
}
