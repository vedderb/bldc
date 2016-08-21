/*
 * Copyright 2016 Andrew Rossignol andrew.rossignol@gmail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef DRIVERS_WS2812_H_
#define DRIVERS_WS2812_H_

#include <cstddef>
#include <cstdint>

namespace drivers {
namespace ws2812 {

/*
 * A color struct that matches the order that the WS2812 LED expects.
 */
struct Color {
  Color() {}

  constexpr Color(uint8_t red, uint8_t green, uint8_t blue)
      : blue(blue), red(red), green(green) {}

  constexpr Color(const Color& color)
      : blue(color.blue), red(color.red), green(color.green) {}

  uint8_t blue;
  uint8_t red;
  uint8_t green;
};

/*
 * Utility colors.
 */
extern const Color black;
extern const Color white;

/*
 * Interpolates between two colors into a destination color, given a ratio from
 * 0.0f to 1.0f.
 */
void InterpolateColor(struct Color *dest_color,
    const struct Color& source_color_a,
    const struct Color& source_colr_b,
    float progress);

/*
 * Initialize the WS2812 driver.
 */
void Init();

/*
 * Sends a frame to the WS2812 LED strip. This makes a copy in the internal bit
 * buffer and returns immediately after starting transmission.
 */
void SendFrame(const ws2812::Color *color_buffer, size_t length);

}  // namespace ws2812
}  // namespace drivers

#endif  // DRIVERS_WS2812_H_
