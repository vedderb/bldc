
#include <stdio.h>
#include <inttypes.h>
#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_partition.h"
#include "driver/uart.h"

#include <lispbm.h>
#include <lbm_image.h>


// ////////////////////////////////////////////////////////////
// Flash storage handling for lbm image

static const esp_partition_t *get_lbm_image_storage_partition(void) {
  return esp_partition_find_first(ESP_PARTITION_TYPE_ANY,
                                 ESP_PARTITION_SUBTYPE_ANY,
                                 "lisp");
}


// ////////////////////////////////////////////////////////////
// Lispbm configuration and initializaion

#define GC_STACK_SIZE          256
#define PRINT_STACK_SIZE       256
#define HEAP_SIZE              4096
#define EXTENSION_STORAGE_SIZE 256

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LBM_MEMORY_SIZE_8K];
static uint32_t bitmap_array[LBM_MEMORY_BITMAP_SIZE_8K];
static lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;

static void done_callback(eval_context_t *ctx) {
  char buf[256];
  lbm_print_value(buf, 256, ctx->r);
  printf("\n# %s\n", buf);
}

static void usleep_callback(uint32_t us) {
  TickType_t t = us / (portTICK_PERIOD_MS * 1000);
  if (t == 0) t = 1;
  vTaskDelay(t);
}

static uint32_t image_size = 0;
static uint32_t *image_addr = NULL;
static esp_partition_mmap_handle_t image_mmap_handle;
static const esp_partition_t *lbm_image_partition;

static bool image_write(uint32_t w, int32_t ix, bool const_heap) {
  uint32_t offset = ix * 4; // byte location into partition
  if (ESP_OK == esp_partition_write(lbm_image_partition, offset, &w, 4)) {
    return true;
  }
  return false;
}

static void eval_thread(void *arg) {
	(void)arg;
	lbm_run_eval();
	vTaskDelete(NULL);
}

static bool startup_lbm(void) {

  if (!lbm_init(heap, HEAP_SIZE,
                memory_array, LBM_MEMORY_SIZE_8K,
                bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    printf("failed initialize lbm\n");
    fflush(stdout);
    esp_restart();
  }

  lbm_set_usleep_callback(usleep_callback);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_printf_callback(printf);
  lbm_set_verbose(true);

  lbm_image_init(image_addr,
                 image_size,
                 image_write);

  if (!lbm_image_exists()) {
    printf("Image does not exist - creating!\n");
    lbm_image_create("v01");
  }
  if (!lbm_image_boot()) {
    printf("Unable to boot image.\n");
    fflush(stdout);
    esp_restart();
  }
  printf("Image booted\n");
  
  lbm_add_eval_symbols();

  xTaskCreatePinnedToCore(eval_thread, "lbm_eval", 3072, NULL, 6, NULL, tskNO_AFFINITY);
  return true;
}

// ////////////////////////////////////////////////////////////
// UART

static void init_uart(void) {
    uart_config_t uart_config = {
      .baud_rate = 115200,
      .data_bits = UART_DATA_8_BITS,
      .parity = UART_PARITY_DISABLE,
      .stop_bits = UART_STOP_BITS_1,
      .flow_ctrl = UART_HW_FLOWCTRL_DISABLE,
      .source_clk = UART_SCLK_DEFAULT,
    };

    uart_driver_install(UART_NUM_0, 512, 0, 0, NULL, 0);
    uart_param_config(UART_NUM_0, &uart_config);
  }

// ////////////////////////////////////////////////////////////
// Put it all together 
void app_main(void)
{
  init_uart();
 
  lbm_image_partition = get_lbm_image_storage_partition();
  if (lbm_image_partition) {
    printf("lbm_image_partition found\n");
    image_size = lbm_image_partition->size / sizeof(uint32_t);
  } else {
    printf("ERROR: cannot find lbm_image_partition\n");
    fflush(stdout);
    esp_restart();
  }

  if (esp_partition_mmap(lbm_image_partition,
                         0,
                         lbm_image_partition->size,
                         ESP_PARTITION_MMAP_DATA,
                         (const void**)&image_addr,
                         &image_mmap_handle) == ESP_OK) {
    printf("Image paritition successfully mapped at %x\n", (unsigned int)image_addr); 
  } else {
    printf("ERROR: cannot mmap image partition\n");
    fflush(stdout);
    esp_restart();
  }
    
  startup_lbm();


  while (true) {
    // HERE read user input 
      
    static char input_buffer[512];
    static int pos = 0;
    uint8_t data;
    int len = uart_read_bytes(UART_NUM_0, &data, 1, 0); // Non-blocking read

    if (len > 0) {
      if (data == '\n' || data == '\r') {
        if (pos > 0) {
          input_buffer[pos] = '\0';

          if (strncmp(input_buffer, ":clear", 6) == 0 ) {
            if (ESP_OK == esp_partition_erase_range(lbm_image_partition,0 ,lbm_image_partition->size)) {
              // restart after clearing
              esp_restart();              
            } else {
              printf("Failed to erase partition\n");
            }
          }
          
          // Process the complete line
          lbm_pause_eval();
          while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
            vTaskDelay(1);
          }

          lbm_create_string_char_channel(&string_tok_state,
                                         &string_tok,
                                         input_buffer);
          lbm_load_and_eval_expression(&string_tok);
          lbm_continue_eval();

          // The input_buffer will now be read by the reader in another thread.
          // Give it some time to do what it does!
          // More robust handling of this inter-thread communication is deisrable.
          vTaskDelay(100 / portTICK_PERIOD_MS);
          
          pos = 0; // Reset for next input
          printf(">"); fflush(stdout);
        }
      } else if (pos < sizeof(input_buffer) - 1) {
        input_buffer[pos++] = data;
        putchar(data); // Echo character
        fflush(stdout);
      }
    }

    vTaskDelay(10 / portTICK_PERIOD_MS);
  }
    
  printf("Restarting now.\n");
    
  esp_restart();
}
    
