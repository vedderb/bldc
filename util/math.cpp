#include "util/math.h"

#include <cmath>

namespace util {

void ApplyDeadband(float *value, float threshold, float max_value) {
  float abs_value = fabsf(*value);
  if (abs_value < threshold) {
    *value = 0.0f;
    return;
  }

  float deadband_value = ((abs_value - threshold) * max_value)
                             / (max_value - threshold);
  if (*value > 0.0f) {
    *value = deadband_value;
  } else {
    *value = -deadband_value;
  }
}

}  // namespace util
