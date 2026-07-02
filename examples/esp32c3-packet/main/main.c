
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_partition.h"
#include "esp32c3/rom/cache.h"
#include "driver/uart.h"

#include <lispbm.h>
#include <lbm_image.h>
#include <lbm_packet_interface.h>


#define FW_BUILD_ID (__DATE__ " " __TIME__)


// ////////////////////////////////////////////////////////////
// Flash storage handling for lbm image

static const esp_partition_t *get_lbm_image_storage_partition(void) {
  return esp_partition_find_first(ESP_PARTITION_TYPE_ANY,
                                 ESP_PARTITION_SUBTYPE_ANY,
                                 "lisp");
}


// ////////////////////////////////////////////////////////////
// Lispbm configuration and initialization

#define GC_STACK_SIZE          256
#define PRINT_STACK_SIZE       256
#define HEAP_SIZE              4096
#define EXTENSION_STORAGE_SIZE 256

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static uint32_t memory_array[LBM_MEMORY_SIZE_8K];
static uint32_t bitmap_array[LBM_MEMORY_BITMAP_SIZE_8K];
static lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

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
  (void)const_heap;
  if (ix < 0 || ix >= (int32_t)image_size) {
    lbm_packet_if_printf("image_write: ix=%d out of range [0,%u)\n", (int)ix, (unsigned)image_size);
    return false;
  }
  uint32_t offset = (uint32_t)ix * 4;
  uint32_t current = 0;
  if (esp_partition_read(lbm_image_partition, offset, &current, 4) != ESP_OK) {
    lbm_packet_if_printf("image_write: read fail ix=%d\n", (int)ix);
    return false;
  }
  if (current == w) return true;
  if (current != 0xFFFFFFFF) {
    lbm_packet_if_printf("image_write: collision ix=%d cur=0x%08x new=0x%08x\n",
                         (int)ix, (unsigned)current, (unsigned)w);
    return false;
  }
  if (esp_partition_write(lbm_image_partition, offset, &w, 4) != ESP_OK) {
    lbm_packet_if_printf("image_write: write fail ix=%d\n", (int)ix);
    return false;
  }
  Cache_Invalidate_Addr((uint32_t)((uint8_t*)image_addr + offset), 4);
  return true;
}

static void eval_thread(void *arg) {
  (void)arg;
  lbm_run_eval();
  vTaskDelete(NULL);
}

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  char buf[256];
  for (lbm_uint i = 0; i < argn; i++) {
    if (lbm_is_array_r(args[i])) {
      const char *str = lbm_dec_str(args[i]);
      if (str) {
        lbm_packet_if_printf("%s", str);
        continue;
      }
    }
    lbm_print_value(buf, sizeof(buf), args[i]);
    lbm_packet_if_printf("%s", buf);
  }
  return ENC_SYM_TRUE;
}

static bool lbm_restart_fn(bool load_code) {
  (void)load_code;
  if (!lbm_init(heap, HEAP_SIZE,
                memory_array, LBM_MEMORY_SIZE_8K,
                bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return false;
  }
  lbm_set_usleep_callback(usleep_callback);
  lbm_set_printf_callback(printf);
  lbm_image_init(image_addr, image_size, image_write);
  esp_partition_erase_range(lbm_image_partition, 0, lbm_image_partition->size);
  lbm_image_create(FW_BUILD_ID);
  if (!lbm_image_boot()) {
    return false;
  }
  lbm_add_eval_symbols();
  lbm_add_extension("print", ext_print);
  return true;
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
  lbm_set_printf_callback(printf);
  lbm_set_verbose(true);

  lbm_image_init(image_addr,
                 image_size,
                 image_write);

  bool need_fresh_image = !lbm_image_exists();
  if (!need_fresh_image) {
    char *stored = lbm_image_get_version();
    if (!stored || strcmp(stored, FW_BUILD_ID) != 0) {
      printf("Image build ID mismatch (stored: %s, expected: %s) - recreating!\n",
             stored ? stored : "none", FW_BUILD_ID);
      need_fresh_image = true;
    }
  }
  if (need_fresh_image) {
    printf("Creating fresh image for build: %s\n", FW_BUILD_ID);
    esp_partition_erase_range(lbm_image_partition, 0, lbm_image_partition->size);
    lbm_image_create(FW_BUILD_ID);
  }
  if (!lbm_image_boot()) {
    printf("Unable to boot image.\n");
    fflush(stdout);
    esp_restart();
  }
  printf("Image booted\n");

  lbm_add_eval_symbols();
  lbm_add_extension("print", ext_print);

  xTaskCreatePinnedToCore(eval_thread, "lbm_eval", 3072, NULL, 6, NULL, tskNO_AFFINITY);
  return true;
}


// ////////////////////////////////////////////////////////////
// UART and packet interface

static void uart_send(uint8_t *data, unsigned int len) {
  uart_write_bytes(UART_NUM_0, (const char *)data, len);
}

static void sleep_ms(uint32_t ms) {
  vTaskDelay(ms / portTICK_PERIOD_MS);
}

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

static void ctx_done_callback(eval_context_t *ctx) {
  (void)ctx;
  lbm_image_save_constant_heap_ix();
  lbm_image_save_global_env();
}

static lbm_packet_if_cfg_t packet_cfg = {
  .hw_name          = "ESP32C3",
  .fw_name          = "LispBM",
  .fw_version_major = 6,
  .fw_version_minor = 6,
  .send             = uart_send,
  .sleep_ms         = sleep_ms,
  .lbm_restart      = lbm_restart_fn,
  .ctx_done         = ctx_done_callback,
};


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
    printf("Image partition successfully mapped at %x\n", (unsigned int)image_addr);
  } else {
    printf("ERROR: cannot mmap image partition\n");
    fflush(stdout);
    esp_restart();
  }

  startup_lbm();

  lbm_packet_if_init(&packet_cfg);

  while (true) {
    uint8_t data;
    int len = uart_read_bytes(UART_NUM_0, &data, 1, 0); // non-blocking
    if (len > 0) {
      lbm_packet_if_process_byte(data);
    }
    vTaskDelay(1 / portTICK_PERIOD_MS);
  }
}
