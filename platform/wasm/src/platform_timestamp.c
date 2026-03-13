#include <emscripten.h>
#include <stdint.h>

uint32_t lbm_timestamp(void) {
  return (uint32_t)(emscripten_get_now() * 1000.0); /* ms -> us */
}
