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

#ifndef UTIL_SINGLETON_H_
#define UTIL_SINGLETON_H_

#include <cstdint>

namespace util {

/*
 * A singleton template that reserves space for an object and provides a means
 * for obtaining the instance. This is primarily intended for objects that
 * provide a service for which there should only be one instance of.
 */
template<typename T>
class Singleton {
 public:
  /*
   * Initializes the singleton by invoking T's constructor with matching
   * arguments.
   */
  template<typename... Args>
  static void Init(Args&&... args);

  /*
   * Obtains a pointer to the underlying object.
   *
   * Note that it is unsafe to call this before calling Init(). The pointer
   * returned will reference a malformed object.
   */
  static T* Instance();

 private:
  /*
   * Allocate storage for the singleton object.
   *
   * sizeof is used in conjunction with uint8_t in order to prevent static
   * initialization. This is explicit through the Init() function provided.
   */
  static uint8_t singleton_storage_[sizeof(T)];
};

} // namespace util

#include "util/singleton_impl.h"

#endif  // UTIL_SINGLETON_H_
