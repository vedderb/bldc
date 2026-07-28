#include "common/system.h"
#include <stdint.h>

extern "C" {
#include "platform_timestamp.h"
}

uint32_t lbm_timestamp(void) {
  return g_system->getMillis() * 1000; /* ms -> us */
}
